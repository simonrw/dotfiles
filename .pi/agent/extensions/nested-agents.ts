import { readFile, realpath } from "node:fs/promises";
import { homedir } from "node:os";
import { dirname, isAbsolute, join, relative, resolve, sep } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

const MESSAGE_TYPE = "nested-agents:instructions";
const MAX_FILE_BYTES = 32 * 1024;
const MAX_READ_BYTES = 128 * 1024;

function isInside(root: string, path: string): boolean {
	const part = relative(root, path);
	return part === "" || (part !== ".." && !part.startsWith(`..${sep}`) && !isAbsolute(part));
}

function isMissing(error: unknown): boolean {
	return error instanceof Error && "code" in error && (error.code === "ENOENT" || error.code === "ENOTDIR");
}

export default function nestedAgents(pi: ExtensionAPI) {
	let delivered = new Set<string>();
	let generation = 0;

	pi.registerFlag("no-nested-agents", {
		description: "Disable automatic nested AGENTS.md instructions",
		type: "boolean",
		default: false,
	});

	function restore(_event: unknown, ctx: ExtensionContext) {
		generation++;
		delivered = new Set();
		// Only messages still in the active, compacted branch count as delivered.
		for (const message of ctx.sessionManager.buildSessionContext().messages) {
			if (message.role !== "custom" || message.customType !== MESSAGE_TYPE) continue;
			const details = message.details as { paths?: unknown } | undefined;
			if (!Array.isArray(details?.paths)) continue;
			for (const path of details.paths) {
				if (typeof path === "string") delivered.add(path);
			}
		}
	}

	pi.on("session_start", restore);
	pi.on("session_tree", restore);
	pi.on("session_compact", restore);
	pi.on("session_shutdown", () => {
		generation++;
		delivered.clear();
	});

	pi.on("tool_result", async (event, ctx) => {
		if (pi.getFlag("no-nested-agents") || event.toolName !== "read" || event.isError) return;
		if (typeof event.input.path !== "string" || !event.input.path) return;
		const currentGeneration = generation;
		const warn = (path: string, error: unknown) => {
			if (ctx.hasUI && currentGeneration === generation) {
				ctx.ui.notify(`Could not load nested instructions from ${path}: ${String(error)}`, "warning");
			}
		};

		let root: string;
		let file: string;
		try {
			let path = event.input.path.replace(/^@/, "");
			if (path === "~" || path.startsWith("~/")) path = join(homedir(), path.slice(1));
			[root, file] = await Promise.all([realpath(ctx.cwd), realpath(resolve(ctx.cwd, path))]);
		} catch (error) {
			if (!isMissing(error)) warn(event.input.path, error);
			return;
		}
		if (!isInside(root, file)) return;

		const directories: string[] = [];
		for (let directory = dirname(file); directory !== root && isInside(root, directory); directory = dirname(directory)) {
			directories.push(directory);
		}

		const instructions: { path: string; content: string }[] = [];
		let remaining = MAX_READ_BYTES;
		for (const directory of directories.reverse()) {
			const candidate = join(directory, "AGENTS.md");
			try {
				const path = await realpath(candidate);
				// Check instruction symlinks too, not just the file being read.
				if (!isInside(root, path) || dirname(path) === root || delivered.has(path)) continue;
				if (instructions.some((instruction) => instruction.path === path)) continue;
				if (remaining === 0) break;
				const bytes = await readFile(path);
				const limit = Math.min(MAX_FILE_BYTES, remaining);
				const truncated = bytes.length > limit;
				// Streaming decoding omits a partial UTF-8 code point at the boundary.
				let content = new TextDecoder().decode(bytes.subarray(0, limit), { stream: truncated });
				remaining -= Math.min(bytes.length, limit);
				if (truncated) content += `\n[Truncated. Read ${path} directly for the complete instructions.]`;
				instructions.push({ path, content });
			} catch (error) {
				if (!isMissing(error)) warn(candidate, error);
			}
		}
		if (currentGeneration !== generation) return;
		// Parallel nested reads may have discovered the same files while we awaited I/O.
		const fresh = instructions.filter(({ path }) => !delivered.has(path));
		if (fresh.length === 0) return;

		// Never attach instructions to tool results: codemode can discard those entirely.
		// triggerTurn:false queues this after the batch, without steering or skipping siblings.
		pi.sendMessage({
			customType: MESSAGE_TYPE,
			content: "Directory instructions for the files just read. Each applies only within its directory subtree; more specific instructions take precedence.\n\n" +
				fresh.map(({ path, content }) => `[Directory Context: ${path}]\n${content}`).join("\n\n"),
			display: false,
			details: { paths: fresh.map(({ path }) => path) },
		}, { triggerTurn: false });
		for (const { path } of fresh) delivered.add(path);
	});
}
