# Nested AGENTS.md for Pi

`.pi/agent/extensions/nested-agents.ts` discovers nested `AGENTS.md` files after successful `read` calls, including calls inside codemode. It sends instructions as separate, hidden conversation messages, never as part of a tool result. Scripts can truncate, discard, or fail after a read without losing the instructions.

## Install

Link the extension into Pi's personal extensions directory, then run `/reload`:

```fish
ln -s ~/dotfiles/.pi/agent/extensions/nested-agents.ts ~/.pi/agent/extensions/nested-agents.ts
```

Alternatively, load it for one session:

```fish
pi -e ~/dotfiles/.pi/agent/extensions/nested-agents.ts
```

Disable with `--no-nested-agents`. Do not load this alongside `pi-nested-agents-md`, which would duplicate instructions.

## Behavior

- Searches from the project root's children down to the file's directory, parent instructions first. Pi already handles the root and its ancestors.
- Uses real paths and rejects reads and instruction symlinks that escape `ctx.cwd`.
- Deduplicates concurrent reads. Restores delivery state from messages remaining in the active branch on resume, reload, tree navigation, and compaction. Compacted-away instructions become eligible on the next read.
- Queues messages with `triggerTurn: false`, so they follow the complete tool batch without steering or skipping sibling tools.
- Limits instruction content to 32 KiB per file and 128 KiB per read, with UTF-8-safe truncation notices. Files skipped because the total budget ran out remain eligible for later discovery.

Instructions reach the model on its next request. A codemode script that reads and then edits in the same call cannot reconsider its edits using newly discovered instructions. Read first, let the model see the instructions, then edit in a separate call. Like the original extension, shell commands and custom file-reading tools are not monitored.

This targets the `@earendil-works/pi-coding-agent` API and was tested against Pi 1.1.0. It is inspired by [pi-nested-agents-md](https://github.com/code-yeongyu/pi-nested-agents-md), with independent message delivery instead of tool-result injection.

## Tests

```fish
node --test tests/pi-nested-agents.mjs
```

Requires Node with TypeScript stripping and Pi on `PATH`, or `PI_PACKAGE_DIR` pointing to its installed package directory. The integration test uses the real Pi session and QuickJS codemode tool with a scripted model, temporary settings, and a fake API key. It makes no model requests and checks the next model context after truncated output, discarded output, parallel reads, and script failure.
