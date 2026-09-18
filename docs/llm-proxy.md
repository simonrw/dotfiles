# Model request logging proxy

Dependency-free TypeScript, run directly with Node 24 or newer. No install or build step. From this repository:

```sh
node cmd/llm-proxy/main.mts --upstream https://chatgpt.com
```

The proxy listens on `127.0.0.1:8765` and prints a readable transcript of each request. System instructions and developer/harness prompts have their own labelled sections, followed by user, assistant and tool messages in their sent order. Text keeps its real line breaks, rather than appearing as escaped JSON strings. Tool definitions, other request fields and redacted headers follow the messages. Nothing is summarised or truncated within the inspection limit.

It recognises Responses API `instructions` and `input`, Chat Completions `messages`, and Anthropic `system` and `messages`. Each heading includes the original field or array index. Harness instructions embedded in ordinary messages, such as AGENTS.md content or environment context, stay visible with the role the client assigned them. Only content actually sent through the proxy is visible; it cannot recover provider-side prompts or history referenced only by an ID.

Each request and its response summary share an ID. Summaries include HTTP status, elapsed time and response bytes. HTTP/SSE responses stream through without buffering. WebSockets are explicitly rejected, so configure SSE as below. Use `--format json` for the original indented JSON view.

For JSONL output:

```sh
umask 077
node cmd/llm-proxy/main.mts --upstream https://chatgpt.com --format jsonl > /tmp/llm-requests.jsonl
```

No log files are created unless you redirect stdout. Startup messages go to stderr. Stop with Ctrl-C. `--help` lists options.

## Codex with ChatGPT login

Keep your existing Codex login. With the proxy running against `https://chatgpt.com`, add this table to your user-level `~/.codex/config.toml`:

```toml
[model_providers.logging]
name = "Local request logger"
base_url = "http://127.0.0.1:8765/backend-api/codex"
wire_api = "responses"
requires_openai_auth = true
supports_websockets = false
```

Start a new Codex process using that provider:

```sh
codex -c 'model_provider="logging"'
```

This keeps your selected model and existing OpenAI authentication. The proxy forwards the credential Codex supplies; it never reads Codex's credential files or 1Password. For a permanent default, set `model_provider = "logging"` at the top level of your user config, before any table headers. Do not put provider settings in a project's `.codex/config.toml`.

Run ordinary `codex` without the override to bypass the proxy. If you set the permanent default, remove it or restore your previous provider. You must restart Codex after changing provider settings.

## Codex with an OpenAI API key

Start the proxy against the API instead:

```sh
node cmd/llm-proxy/main.mts --upstream https://api.openai.com
```

Use this provider table instead of the ChatGPT version:

```toml
[model_providers.logging]
name = "Local request logger"
base_url = "http://127.0.0.1:8765/v1"
wire_api = "responses"
env_key = "OPENAI_API_KEY"
supports_websockets = false
```

Remove `requires_openai_auth` if switching from the ChatGPT example. Supply `OPENAI_API_KEY` through your existing credential workflow, then use the same `codex -c 'model_provider="logging"'` command. API-key requests use API billing, not your ChatGPT subscription.

These settings follow the [official OpenAI advanced configuration documentation](https://developers.openai.com/codex/config-advanced) and [configuration reference](https://developers.openai.com/codex/config-reference). Live authenticated calls have not been tested; verification uses local mock providers and dummy credentials.

## Pi

For your `openai-codex` provider, run the proxy against `https://chatgpt.com`. Merge this into `~/.pi/agent/models.json`, preserving other entries:

```json
{
  "providers": {
    "openai-codex": {
      "baseUrl": "http://127.0.0.1:8765/backend-api"
    }
  }
}
```

Also set `"transport": "sse"` in `~/.pi/agent/settings.json`, then restart Pi. Existing OAuth login still supplies authentication. Pi appends `/codex/responses` to that base URL. These settings were checked against the installed Pi provider and its `docs/models.md` and `docs/settings.md`.

For Pi's API-key `openai` provider, override its `baseUrl` to `http://127.0.0.1:8765/v1` and run the proxy against `https://api.openai.com`. For `anthropic`, use `http://127.0.0.1:8765` and upstream `https://api.anthropic.com`. Run separate proxy processes on different `--port` values for simultaneous upstreams. Remove the base URL overrides and restore your prior transport to bypass it.

## Scope and redaction

- Credentials still travel unchanged to the fixed upstream over verified HTTPS. Local HTTP is loopback-only. No TLS interception or custom certificates, and no `HTTP_PROXY`/`HTTPS_PROXY` setup.
- Logs hide all header values except content type, length, encoding and accept. They hide query strings and recursively redact credential fields, including JSON-encoded tool arguments. Known header, query and body credentials are also scrubbed from repeated text. Common API-key, bearer/basic, JWT, private-key and credential-assignment patterns are scrubbed.
- Arbitrary secrets in prose, unknown formats, encoded blobs, files or images cannot be reliably detected. Logs still contain prompts and code. Treat logs and terminal scrollback as sensitive; do not publish them without review.
- JSON request inspection supports plain, gzip, deflate, Brotli and zstd bodies. Both compressed and decoded inspection size are bounded by `--max-body`, default 16 MiB. Oversized, invalid, deeply nested or non-JSON bodies are omitted from logs but still forwarded unchanged. A visible `BODY NOT SHOWN` section explains omissions. Increase the limit, up to 64 MiB, if full context requests are omitted.
- Response bodies and headers are not logged. This captures model traffic routed to this provider, not OAuth refreshes, MCP, shell commands or every Codex/Pi network request. Browser requests are rejected. Other local processes can still reach the listener while it runs.

## Tests

```sh
node --test cmd/llm-proxy/*.test.mts
```

Tests use loopback mock servers and dummy credentials. They cover credential redaction, unchanged forwarding, compressed requests, inspection limits, SSE delivery, cancellation, errors, concurrent requests and listener safeguards.
