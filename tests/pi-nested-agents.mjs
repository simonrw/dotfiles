import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdtemp, mkdir, realpath, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { pathToFileURL } from "node:url";
import { test } from "node:test";
import nestedAgents from "../.pi/agent/extensions/nested-agents.ts";

async function fixture(t) {
	const temporary = await realpath(await mkdtemp(join(tmpdir(), "pi-nested-agents-")));
	t.after(() => rm(temporary, { recursive: true, force: true }));
	const cwd = join(temporary, "repo");
	await mkdir(join(cwd, "src", "deep"), { recursive: true });
	await writeFile(join(cwd, "AGENTS.md"), "ROOT_RULE");
	await writeFile(join(cwd, "src", "AGENTS.md"), "PARENT_RULE");
	await writeFile(join(cwd, "src", "deep", "AGENTS.md"), "CHILD_RULE");
	await writeFile(join(cwd, "src", "deep", "file.txt"), "file contents");
	return { temporary, cwd };
}

function harness(cwd, disabled = false) {
	const handlers = new Map();
	const messages = [];
	const ctx = { cwd, hasUI: false, sessionManager: { buildSessionContext: () => ({ messages }) } };
	nestedAgents({
		on: (event, handler) => handlers.set(event, handler),
		registerFlag() {},
		getFlag: () => disabled,
		sendMessage(message, options) {
			assert.deepEqual(options, { triggerTurn: false });
			messages.push({ role: "custom", ...message });
		},
	});
	const emit = (name, event = {}) => handlers.get(name)?.(event, ctx);
	const read = (path = "src/deep/file.txt", extra = {}) => emit("tool_result", {
		toolName: "read", input: { path }, isError: false,
		content: [{ type: "text", text: "file contents" }], ...extra,
	});
	return { messages, emit, read };
}

test("parallel reads send one outermost-first message without modifying results", async (t) => {
	const { cwd } = await fixture(t);
	const h = harness(cwd);
	const results = await Promise.all([h.read(), h.read(), h.read()]);
	assert.deepEqual(results, [undefined, undefined, undefined]);
	assert.equal(h.messages.length, 1);
	assert.match(h.messages[0].content, /PARENT_RULE[\s\S]*CHILD_RULE/);
	assert.doesNotMatch(h.messages[0].content, /ROOT_RULE/);
	await h.emit("session_start");
	await h.read();
	assert.equal(h.messages.length, 1, "resume retains delivery state from visible context");
	await h.emit("session_compact");
	await h.read();
	assert.equal(h.messages.length, 1, "retained instructions aren't duplicated");
	h.messages.length = 0;
	await h.emit("session_compact");
	await h.read();
	assert.equal(h.messages.length, 1, "compacted instructions are eligible again");
	h.messages.length = 0;
	await h.emit("session_tree");
	await h.read();
	assert.equal(h.messages.length, 1, "abandoned branches don't suppress discovery");
});

test("ignores failed reads, other tools, disabled mode, outside paths and symlink escapes", async (t) => {
	const { temporary, cwd } = await fixture(t);
	const outside = join(temporary, "repo-other");
	await mkdir(outside);
	await writeFile(join(outside, "AGENTS.md"), "OUTSIDE_RULE");
	await writeFile(join(outside, "file.txt"), "outside");
	await symlink(outside, join(cwd, "escape"));
	await mkdir(join(cwd, "linked"));
	await symlink(join(outside, "AGENTS.md"), join(cwd, "linked", "AGENTS.md"));
	await writeFile(join(cwd, "linked", "file.txt"), "inside");
	const h = harness(cwd);
	await h.read(undefined, { isError: true });
	await h.read(undefined, { toolName: "bash" });
	await h.read(join(outside, "file.txt"));
	await h.read("escape/file.txt");
	await h.read("linked/file.txt");
	assert.equal(h.messages.length, 0);
	const disabled = harness(cwd, true);
	await disabled.read();
	assert.equal(disabled.messages.length, 0);
	await h.read();
	assert.equal(h.messages.length, 1, "failed discovery never poisons the cache");
});

test("explicit instruction reads still inject, and truncation preserves UTF-8", async (t) => {
	const { cwd } = await fixture(t);
	await writeFile(join(cwd, "src", "AGENTS.md"), "x" + "🙂".repeat(9000));
	const h = harness(cwd);
	await h.read("@src/AGENTS.md");
	assert.equal(h.messages.length, 1);
	assert.match(h.messages[0].content, /Truncated/);
	assert.doesNotMatch(h.messages[0].content, /�/);
});

test("real Pi codemode cannot discard instructions, even on script failure", async (t) => {
	// Use the installed Pi SDK, without installing dependencies or using real credentials.
	const sdkUrl = process.env.PI_PACKAGE_DIR
		? pathToFileURL(join(process.env.PI_PACKAGE_DIR, "dist", "index.js")).href
		: execFileSync(process.execPath, ["--input-type=module", "-e", "console.log(import.meta.resolve('@earendil-works/pi-coding-agent'))"], {
			cwd: dirname(await realpath(execFileSync("which", ["pi"], { encoding: "utf8" }).trim())),
			encoding: "utf8",
		}).trim();
	const { createAgentSession, createCodemodeExtension, DefaultResourceLoader, ModelRuntime, SessionManager, SettingsManager } = await import(sdkUrl);
	const { cwd, temporary } = await fixture(t);
	const agentDir = join(temporary, "agent");
	await mkdir(agentDir);
	const settingsManager = SettingsManager.inMemory({
		defaultTools: ["read", "codemode"],
		compaction: { enabled: false }, retry: { enabled: false },
	});
	const resourceLoader = new DefaultResourceLoader({
		cwd, agentDir, settingsManager,
		noExtensions: true, noSkills: true, noPromptTemplates: true, noThemes: true, noContextFiles: true,
		extensionFactories: [createCodemodeExtension(), nestedAgents],
	});
	await resourceLoader.reload();
	const modelRuntime = await ModelRuntime.create({
		authPath: join(agentDir, "auth.json"), modelsPath: null,
		modelsStorePath: join(agentDir, "models-cache"), refreshOnCreate: false,
	});
	await modelRuntime.setRuntimeApiKey("anthropic", "fake-test-key");
	const model = modelRuntime.getModels("anthropic")[0];
	assert.ok(model, "Pi must include a static Anthropic model");
	const sessionManager = SessionManager.inMemory(cwd);
	const { session } = await createAgentSession({
		cwd, agentDir, model, modelRuntime, resourceLoader, settingsManager, sessionManager, thinkingLevel: "off",
	});
	t.after(() => session.dispose());
	await session.bindExtensions({});

	for (const script of [
		'const content = await tools.read({path: "src/deep/file.txt"}); text(content.slice(0, 4));',
		'await Promise.all([tools.read({path: "src/deep/file.txt"}), tools.read({path: "src/deep/file.txt"})]);',
		'await tools.read({path: "src/deep/file.txt"}); throw new Error("after read");',
	]) {
		// A fresh branch ensures each case must discover and deliver the instructions itself.
		sessionManager.resetLeaf();
		await session.bindExtensions({});
		const requests = [];
		session.agent.streamFunction = (_model, context) => {
			requests.push(structuredClone(context));
			const first = requests.length === 1;
			const message = {
				role: "assistant", api: model.api, provider: model.provider, model: model.id,
				content: first ? [{ type: "toolCall", id: "call-1", name: "codemode", arguments: { code: script } }] : [{ type: "text", text: "done" }],
				stopReason: first ? "toolUse" : "stop", timestamp: Date.now(),
				usage: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, totalTokens: 0, cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 } },
			};
			return {
				async *[Symbol.asyncIterator]() { yield { type: "done", reason: message.stopReason, message }; },
				result: async () => message,
			};
		};
		await session.prompt("Read the file.");
		assert.equal(requests.length, 2, "no extra turn is triggered");
		const nextRequest = JSON.stringify(requests[1]);
		assert.match(nextRequest, /PARENT_RULE/);
		assert.match(nextRequest, /CHILD_RULE/);
		const messages = sessionManager.buildSessionContext().messages;
		const custom = messages.filter((m) => m.role === "custom");
		assert.equal(custom.length, 1);
		const resultIndex = messages.findIndex((m) => m.role === "toolResult");
		assert.ok(resultIndex < messages.indexOf(custom[0]), "instructions follow the outer tool result");
		assert.doesNotMatch(JSON.stringify(messages[resultIndex].content), /PARENT_RULE|CHILD_RULE/);
	}
});
