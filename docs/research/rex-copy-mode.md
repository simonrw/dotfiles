# tmux-style copy mode for Rex

## Question

Can `Ctrl+s` then `[` enter a mode with Vim movement, Space to start a region, and Enter to copy it and return to the terminal?

Researched 2026-10-22. Research only. No configuration, terminal input, selection, clipboard, or implementation changes were made. Read-only CLI discovery was run against the existing server. The behavior tests below have not been run.

## Findings

A native implementation is not available through the documented Lua API and the installed client's advertised actions. Key modes solve input routing, but the missing parts are a movable copy cursor, selection anchors, viewport control, and selected-text extraction/copy. This is a conclusion about the inspected version, not proof that Rex has no internal selection implementation. See [installed evidence](#installed-evidence), [Lua reference](https://www.superlogical.com/rex/docs/reference/lua), and [action discovery](https://www.superlogical.com/rex/docs/customize/keyboard-shortcuts#finding-actions).

The smallest practical proposal is a snapshot viewer: a Rex custom action captures the source terminal and opens an isolated Neovim instance with the requested keys. This needs Lua configuration plus a helper/viewer script, but no proposed Rex core change. It copies a frozen text snapshot in a separate pane or layer, rather than selecting in the original terminal. Native in-place behavior needs Rex client changes or new upstream actions. This design is inferred from the [format schema](#installed-evidence), Rex's [custom actions](https://www.superlogical.com/rex/docs/customize/actions), [layout APIs](https://www.superlogical.com/rex/docs/reference/lua#rexlayout), and Neovim's [visual selection](https://raw.githubusercontent.com/neovim/neovim/v0.11.5/runtime/doc/visual.txt#L16-L31).

## Identity and local baseline

This is Superlogical Rex, the multiplexer with a self-contained macOS client and local server, not an unrelated project named rex. The installed app is Rex Beta `0.1.0 (1347)`. The embedded CLI and running server report `0.0.1792607779-g171246`, commit `1712469ff26fb10b37b53ea6f60d5a86f8ba9f3a`, API revision 5. The app's plist records a different `RexSourceCommit`, `6de2d2bd72c9dacb743c49e495f1c2395998601f`. Preserve both identities when reporting upstream. Sources: [official installation](https://www.superlogical.com/rex/docs/installation), [installed evidence](#installed-evidence).

The launcher checks two older locations, but the installed executable is `/Applications/Rex Beta.app/Contents/Helpers/Rex Server.rex/Contents/MacOS/rex`. Neither launcher candidate exists on this machine. A later implementation must first repair executable discovery. Sources: [.bin/rex:5-15](../../.bin/rex#L5-L15), [installed evidence](#installed-evidence).

The current Rex config sets `local prefix = "ctrl+s>"`, already has Vim pane-focus bindings, and has no copy-mode binding. The existing tmux config uses `C-s`, `mode-keys vi`, and explicitly maps Enter to `copy-selection-and-cancel`. Sources: [Rex config:1-33](../../.config/rex/init.lua#L1-L33), [tmux config:27-34](../../.config/tmux/tmux.conf#L27-L34), [Enter override:129-131](../../.config/tmux/tmux.conf#L129-L131).

The current tests cover session wrappers with a stateful CLI stub and a live fzf picker using `capture`; they do not exercise GUI selection or clipboard behavior. Sources: [session stub:14-56](../../tests/rex-sessions.py#L14-L56), [session tests:59 onward](../../tests/rex-sessions.py#L59), [live picker:23-47](../../tests/rex-fzf.py#L23-L47). Neither test suite was run for this research.

No Rex source checkout was found in the inspected development/work directories or its Go module cache. Public verification of [github.com/superlogical/rex](https://github.com/superlogical/rex) returned 404, `git ls-remote` reported repository not found, and the [public organization repository API](https://api.github.com/orgs/superlogical/repos?per_page=100) returned an empty list. That limits this investigation to official docs and the installed server's self-described API; it does not establish licensing or whether private source exists. No Rex source permalink can responsibly be supplied. The binary's Go module identity is `go.superlogical.com/rex`; see [installed evidence](#installed-evidence).

## Verified API and input handling

- Sequences use `>`, so the entry key would be `ctrl+s>[`. Characters use their unshifted form, and `space`, `enter`, and `escape` are valid named keys. App-recorded shortcuts take precedence over Lua config. Sources: [sequences](https://www.superlogical.com/rex/docs/customize/key-sequences), [keyboard syntax and precedence](https://www.superlogical.com/rex/docs/customize/keyboard-shortcuts#writing-keys).
- `rex.mode("copy", { exclusive = true })` can prevent unbound keys from reaching the shell. `client.mode.enter` takes `name` and optional `once`; `client.mode.exit` leaves the current mode. Modes can stack. Escape exits unless overridden. Mode bindings must name actions, so define custom actions rather than binding anonymous Lua functions inside a mode. Source: [key modes](https://www.superlogical.com/rex/docs/customize/key-modes).
- `rex.bind(key, fn)` calls `fn(ctx, ev)` and consumes input only when it returns `true`. `rex.action{ name, title, run }` exposes a named action with context including `session_id`, `block_id`, and `client_id`. `rex.client.queue(name, args)` requests client actions after an action returns. `rex.block.call("com.superlogical.terminal", "format", args)` returns `{ content = ... }`; it defaults `block_id` to the current block. Sources: [Lua reference](https://www.superlogical.com/rex/docs/reference/lua#key-bindings), [custom actions](https://www.superlogical.com/rex/docs/customize/actions), [installed evidence](#installed-evidence).
- The installed `format` schema promises screen and scrollback, supports `text`, `html`, and `vt`, and accepts `unwrap` for soft-wrap joining and `trim` for trailing spaces. It exposes no range, history limit, paging, stable cell IDs, selection, or viewport argument. CLI `capture --help` instead describes the current screen and describes `--trim` as removing trailing blank lines. Treat history extent and trimming as unresolved until tested, rather than equating schema text with verified capture output. Sources: [installed evidence](#installed-evidence), [server API discovery](https://www.superlogical.com/rex/docs/automate/server-api).
- The connected macOS client's complete action list contains modes, find, prompt jumps, and `pane.send_key`, but no selection/copy-cursor/row-scroll actions. `pane.send_key` sends input to the focused terminal; it cannot substitute for moving a viewer cursor. Server `write` likewise types into the running program. `client.perform` rejects an action absent from the target client's advertised list. Sources: [installed evidence](#installed-evidence), [action discovery](https://www.superlogical.com/rex/docs/customize/keyboard-shortcuts#finding-actions).
- `com.superlogical.terminal.clipboard_read` retrieves a program-originated clipboard write cached by the server, not the user's current selection or a clipboard setter. `terminal.clipboard_written` is a program clipboard event. No generic clipboard setter appears in the inspected action lists. Sources: [installed evidence](#installed-evidence), [events reference](https://www.superlogical.com/rex/docs/reference/events).

Do not assume Ghostty keybinding names work in Rex. Ghostty documents `copy_to_clipboard` and `adjust_selection`, but those are Ghostty application actions. Even Ghostty's `adjust_selection` requires an existing selection. They are absent from this Rex client's advertised list. Sources: [Ghostty action reference](https://ghostty.org/docs/config/keybind/reference#adjust_selection), [installed evidence](#installed-evidence).

The live schema also exposes `client.set_script`, with per-client Lua function bindings. Public Lua docs currently say script bindings have no effect. This version discrepancy merits upstream clarification, but neither API supplies the missing selection/viewport operations. Sources: [installed evidence](#installed-evidence), [Lua reference](https://www.superlogical.com/rex/docs/reference/lua#rexbindkey-fn).

## tmux reference behavior

Tmux enters copy mode with prefix then `[`, navigates screen/history, and copies into a tmux paste buffer. In vi mode Space starts selection and `h/j/k/l` move the copy cursor. The local Enter override copies selection and exits. OS clipboard delivery is a separate concern governed by terminal support and `set-clipboard` or a copy command. Sources: [tmux manual, modes](https://man.openbsd.org/tmux.1#WINDOWS_AND_PANES), [clipboard option](https://man.openbsd.org/tmux.1#set-clipboard), [local Enter override](../../.config/tmux/tmux.conf#L129-L131).

Pin the reference rather than assuming today's default Enter binding: tmux commit `3ad4b2b9539ef49be44936137fdeeb5a0796c092` binds Enter to `copy-pipe-and-cancel`, Space to `begin-selection`, and `h/j/k/l` to cursor movement. Sources: [key-bindings.c:665-671](https://github.com/tmux/tmux/blob/3ad4b2b9539ef49be44936137fdeeb5a0796c092/key-bindings.c#L665-L671), [movement:712-716](https://github.com/tmux/tmux/blob/3ad4b2b9539ef49be44936137fdeeb5a0796c092/key-bindings.c#L712-L716).

## Proposed smallest implementation

This is a plan, not tested code.

1. Repair `.bin/rex` discovery and use a named `copy_mode` action behind `prefix .. "["`. Capture explicit source session/block/client IDs before opening anything. Fetch one `format` text snapshot with deliberate `trim` and `unwrap` choices, and check errors. The required identifiers, capture and layout operations exist in the [Lua API](https://www.superlogical.com/rex/docs/reference/lua) and [installed schema](#installed-evidence).
2. Launch a clean Neovim viewer in a temporary Rex layer or pane. Pass content through a private temporary file or another explicit data channel, never through shell interpolation. Disable user configuration, swap/history persistence, edits, and file modelines. Start near the end. Map normal-mode Space to characterwise Visual mode, retain Vim motions, and map Visual Enter to copy then close. Map normal Enter with no selection to a defined no-op; map `q`/Escape to cancellation. Neovim supplies inclusive characterwise regions and existing movement/selection behavior, avoiding a new Unicode editor. Sources for the underlying selection mechanism: [Neovim visual docs](https://raw.githubusercontent.com/neovim/neovim/v0.11.5/runtime/doc/visual.txt#L16-L31). Mapping, lifecycle and persistence settings still need implementation and review.
3. For the first local macOS version, explicitly choose the Neovim `+` clipboard provider and verify `pbcopy` delivery before closing. A clipboard failure should preserve the selection and report the error. Neovim depends on an external provider, with `pbcopy`/`pbpaste` and OSC 52 among its documented backends. This is not a promise of remote clipboard delivery. Source: [Neovim clipboard providers](https://raw.githubusercontent.com/neovim/neovim/v0.11.5/runtime/doc/provider.txt#L153-L190).
4. Close only the viewer's recorded block/layer, restore the source focus when it still exists, and clean up the snapshot on success, cancellation, process failure and source closure. Prevent duplicate viewers for the same requesting client/source. Rex provides close/focus operations, but their complete viewer lifecycle remains untested. Sources: [session methods](https://www.superlogical.com/rex/docs/reference/lua#rexsessionmethodargs), [configuration execution on the server](https://www.superlogical.com/rex/docs/customize/config).

If selecting directly in the original pane is mandatory, ask upstream for client actions to enter/exit copy mode, move a copy cursor, set an anchor, scroll to the cursor, and copy-and-exit. Keep cursor, viewport, and clipboard state local to the requesting client. Reuse the client's terminal selection machinery if upstream source confirms it is suitable. These are proposed action responsibilities, not existing Rex names. Existing mode bindings could then provide the requested keys. The missing advertised actions are the reason this route requires upstream work; whether shared terminal/core changes are also needed cannot be established without Rex source. Sources: [installed evidence](#installed-evidence), [key modes](https://www.superlogical.com/rex/docs/customize/key-modes).

## Risks and decisions to validate

These are implementation risks inferred from the cited APIs, not observed failures.

- Unicode: test combining marks, CJK width, emoji sequences, tabs and reversed regions. Do not slice Lua strings by bytes to model terminal cells. Let the viewer handle selection, then compare copied bytes with fixtures. The format API returns a string rather than cell coordinates; Neovim distinguishes inclusive/exclusive selection. Sources: [installed schema](#installed-evidence), [visual semantics](https://raw.githubusercontent.com/neovim/neovim/v0.11.5/runtime/doc/visual.txt#L16-L31).
- Wrapping/history: `unwrap=true` preserves logical lines but changes visual rows and therefore vertical motion relative to the original terminal. `unwrap=false` risks artificial newlines on copy. No wrap metadata or paged history is exposed by the inspected format schema. Establish retained-history extent, truncation, memory use and capture latency before selecting a default. Source: [installed schema](#installed-evidence).
- Resize/continuing output: freeze the captured text while the source keeps running. Resizing the viewer should not change snapshot bytes. A native implementation must keep anchors valid across reflow/history eviction and define whether it freezes the view or the underlying data. The snapshot approach deliberately avoids live anchor tracking. Source for available capture/size operations: [installed schema](#installed-evidence).
- Alternate screen: define whether copy mode captures only the active alternate screen or also primary-screen history. The current schema does not specify this. `com.superlogical.terminal.modes` can report alternate-screen state, but that does not establish capture semantics. Source: [installed evidence](#installed-evidence).
- Multiple clients: the config runs on the server; layout operations act on sessions, and terminal sizing has an owner. A helper layer/pane should therefore be treated as session-visible until verified otherwise. Do not assume a new layer is a private client overlay. Preserve explicit IDs and test simultaneous clients before presenting this as native copy mode. Sources: [config location/execution](https://www.superlogical.com/rex/docs/customize/config), [session layouts](https://www.superlogical.com/rex/docs/reference/lua#rexsessionmethodargs), [installed size schema](#installed-evidence).
- Clipboard/remote use: local `pbcopy` affects the machine running the helper, which may be the remote Rex server. OSC 52 is a possible client-delivery route, but Rex permissions, event routing, large payloads and which connected clients receive the write need testing. Avoid using `clipboard_read` as selection extraction. Sources: [Neovim providers](https://raw.githubusercontent.com/neovim/neovim/v0.11.5/runtime/doc/provider.txt#L153-L190), [Rex clipboard events](https://www.superlogical.com/rex/docs/reference/events), [installed evidence](#installed-evidence).

## Meaningful validation

Before implementation, establish capture semantics in a disposable session with numbered lines exceeding screen height, soft wraps, trailing spaces, a blank final line and a known Unicode string. Compare `format` and CLI `capture` with every `trim`/`unwrap` combination and repeat inside an alternate-screen application. Measure the retained output and large-history behavior. These checks resolve the [schema/help ambiguity](#installed-evidence).

For a later implementation, assert the complete key sequence, exact clipboard bytes, inclusive/reversed/multiline selection, Enter without a selection, cancellation without clipboard changes, return of focus, and no leaked keys to a source process that logs input. Exercise resize, ongoing output, viewer/source closure, config reload, missing Neovim/provider, clipboard failure, two attached clients and a remote server. Check temporary-file cleanup on every exit. Use `rex config check` for syntax and keymap/action discovery for installed-version compatibility; these do not prove selection works. Sources: [config validation](https://www.superlogical.com/rex/docs/customize/config#validation), [keymap inspection](https://www.superlogical.com/rex/docs/customize/keyboard-shortcuts#inspecting-bindings), [existing live-test pattern](../../tests/rex-fzf.py#L23-L47).

## Installed evidence

Read-only observations from this machine on 2026-10-22. Commands used the absolute embedded binary because the repo launcher misses it. These are API/help observations, not behavioral copy-mode experiments. All live queries included `--autostart=false`.

```text
Executable: /Applications/Rex Beta.app/Contents/Helpers/Rex Server.rex/Contents/MacOS/rex

$ rex version
rex 0.0.1792607779-g171246
commit: 1712469ff26fb10b37b53ea6f60d5a86f8ba9f3a
built: 2026-10-21T18:36:19Z
api: revision 5 (supports 0 to 5)

$ PlistBuddy CFBundleShortVersionString / CFBundleVersion / RexSourceCommit
app and server Info.plist: 0.1.0 / 1347 / 6de2d2bd72c9dacb743c49e495f1c2395998601f

$ rex api call server.version
{"rex_version":"0.0.1792607779-g171246","platform":"darwin_arm64","commit":"1712469ff26fb10b37b53ea6f60d5a86f8ba9f3a","built_at":"2026-10-21T18:36:19Z","dirty":false,"api_revision":5}

$ go version -m <embedded rex> (identity excerpt)
path go.superlogical.com/rex/cmd/rex
mod go.superlogical.com/rex v0.0.0-20261021183619-1712469ff26f

$ rex -C walker-s actions --json (all names)
client.window.new
client.window.close
client.window.fullscreen.toggle
client.close
client.palette.toggle
client.settings
client.diagnostics
client.version.copy
client.debug.copy
client.pair_device
client.update.accept
client.notice.dismiss
client.shortcuts.edit
client.agents.setup
client.server.restart
session.new
session.switch
session.select
session.next
session.previous
session.rename
session.close
session.refresh
session.open_on_web
client.host.switch
client.host.add
client.host.remove
client.tab.new
client.tab.close
client.tab.rename
client.tab.next
client.tab.previous
client.tab.goto
client.tab.move
client.tab.move.backward
client.tab.move.forward
client.tab.move_to_new_window
pane.split
pane.split.left
pane.split.right
pane.split.up
pane.split.down
pane.split.auto
pane.focus
pane.focus.left
pane.focus.right
pane.focus.up
pane.focus.down
pane.focus_next
pane.focus_previous
pane.move
pane.move.left
pane.move.right
pane.move.up
pane.move.down
pane.zoom
pane.close
pane.balance
pane.resize
pane.move_to_new_tab
pane.move_to_tab
pane.go_to_directory
pane.insert_path
pane.send_key
client.key.forward
client.prompt.previous
client.prompt.next
client.mode.enter
client.mode.exit
client.mode.exit_all
client.find.open
client.find.next
client.find.previous
client.font.increase
client.font.decrease
client.font.reset
client.sidebar.toggle
client.sidebar.activity.toggle
client.tab_style.toggle
client.theme.change
client.theme.import
client.appearance.switch_mode
client.config.copy_path
client.config.reload

$ rex actions --json (all server/block names)
com.superlogical.terminal.clear
com.superlogical.terminal.clipboard_read
com.superlogical.terminal.format
com.superlogical.terminal.input_mode
com.superlogical.terminal.list_dir
com.superlogical.terminal.modes
com.superlogical.terminal.process
com.superlogical.terminal.program_status
com.superlogical.terminal.pwd
com.superlogical.terminal.reset
com.superlogical.terminal.resize
com.superlogical.terminal.set_theme
com.superlogical.terminal.size
com.superlogical.terminal.title
com.superlogical.terminal.write

$ rex api describe com.superlogical.terminal.format (schema excerpts)
Get the screen and scrollback as plain text, HTML, or escape sequences.
format: text, html, vt. Defaults to text.
trim: Remove spaces from the end of each line.
unwrap: Join lines that the terminal wrapped because they were too long,
        so each line of output is one line the program wrote.
Complete input property names:
extra_charsets, extra_cursor, extra_hyperlink, extra_keyboard,
extra_kitty_keyboard, extra_modes, extra_palette, extra_protection,
extra_pwd, extra_scrolling_region, extra_style, extra_tabstops,
format, trim, unwrap.
additionalProperties: false
Output: required content string, "The screen and scrollback in the requested format."
Call with session_id, block_id and method arguments under args.

$ rex api describe client.set_script (description/validation excerpts)
Load Lua scripts for this client, replacing any it loaded before.

Method: client.set_script
Validation notes:
- The scripts belong to this client and apply in every session it's attached to. A request that includes `session_id` or `client_id` fails with `bad_request`.
- To see every key and key sequence the scripts bound to a function, call `client.keymap`. It reports each one as a binding for `client.key.forward`.
- The keys that reach the scripts are fixed when they load. A function that a script binds later, while it runs, receives a key only if a function was already bound to that key or sequence when the scripts loaded.

$ rex api describe client.key (description/validation excerpts)
Send a key to a block, giving Lua key bindings the first chance at it.

Method: client.key
Validation notes:
- Without `block_id`, the key goes to the session's focused block. The server reads it once, before any Lua function runs, so a function that moves focus doesn't change where the key goes. Either way, Lua functions see the block the key goes to as `ctx.block_id`.
- The key goes first to the Lua functions bound to it in this client's scripts, then to those in the configuration file. If none of them takes the key, it goes to the block.
- Only a key bound to a Lua function reaches Lua, and only when it's pressed or repeating. Every other key goes straight to the block. `client.keymap` reports the bound keys as bindings for `client.key.forward`.
- A key with a `prefix` reaches only the functions bound to that exact sequence. A key without one reaches only the functions bound to that key alone.
- A Lua function bound to a key may run for 5 seconds. If it raises an error or runs longer, the call fails and the key doesn't reach the block.

$ rex api describe client.perform (description/validation excerpts)
Ask another client to perform one of its actions and wait for its answer.

Method: client.perform
Validation notes:
- If the client has listed its actions and `action` isn't one of them, the call fails with `not_supported`. A client that listed none is asked anyway, and it may answer `not_supported` itself.

$ rex api describe com.superlogical.terminal.clipboard_read (description/validation excerpts)
Read text that a program in the terminal copied to the clipboard. The server keeps each copy for about a minute, so a client can read text too large to send in an event.

Method: com.superlogical.terminal.clipboard_read
Validation notes:
- Call it as `com.superlogical.terminal.clipboard_read` with `session_id`, `block_id`, and this method's own arguments under `args`.

$ rex api describe com.superlogical.terminal.size (description/validation excerpts)
Get the terminal's size, its owner, and the size each client last requested.

Method: com.superlogical.terminal.size
Validation notes:
- Call it as `com.superlogical.terminal.size` with `session_id`, `block_id`, and this method's own arguments under `args`.

$ rex api describe com.superlogical.terminal.modes (description/validation excerpts)
List the terminal modes that programs can turn on, such as the alternate screen, bracketed paste, and mouse reporting, with each mode's name, number, and whether it is on. It doesn't wake an idle terminal.

Method: com.superlogical.terminal.modes
Validation notes:
- Call it as `com.superlogical.terminal.modes` with `session_id`, `block_id`, and this method's own arguments under `args`.

$ rex capture --help (relevant excerpts)
Print the current screen contents of a terminal block.
Output always ends with a newline.
--trim remove trailing blank lines
--unwrap join soft-wrapped lines
```
