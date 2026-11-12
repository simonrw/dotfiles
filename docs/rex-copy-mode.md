# Rex copy mode

Press `Ctrl+s`, then `[` to browse the focused terminal's screen and scrollback, select text with Vim keys, and copy it to the macOS clipboard. Reload the Rex config with `Ctrl+s`, then `r` after changing it.

| Keys | Action |
| --- | --- |
| `h` `j` `k` `l` | Move by cell, `j`/`k` move by screen row through wrapped lines |
| `w` `b` `e`, `W` `B` `E` | Word and WORD motions |
| `{` `}` | Previous or next blank line |
| `0` `^` `$` | Start, first non-blank, end of line |
| `gg` `G`, `5G` | First, last or numbered line |
| `H` `M` `L` | Top, middle, bottom of the screen |
| `Ctrl+d` `Ctrl+u` `Ctrl+f` `Ctrl+b` `Ctrl+e` `Ctrl+y` | Scroll |
| `/` `?` `n` `N` | Search forward or backward, smartcase |
| `v` or Space, `V`, `o` | Characterwise or linewise selection, swap selection ends |
| `y` or Enter | Copy the selection and close |
| Escape | Clear the selection, or close when nothing is selected |
| `q` | Close |

Motions take counts, for example `3w` or `2}`.

The viewer is `cmd/rex-copy-mode`, a Bubble Tea program that `mise run go:build` builds into `.bin`. The `copy_mode` action in `.config/rex/init.lua` puts the viewer in the source pane's place in the layout, including zoom, and detaches the source, which keeps running. When the viewer exits, `copy_mode_restore` puts the source back and closes the viewer. Rex has no API for a program to take over another terminal's screen, so the viewer is a separate block. Closing the viewer pane directly instead of quitting leaves the source detached; `session.list_blocks` still lists it. It captures the source with Rex's `vt` format, so colors and styles match the terminal. The source keeps running, while the snapshot stays fixed. Soft-wrapped rows become logical lines, so copying a long line adds no artificial newlines.

A clipboard failure is shown in the status line and keeps the selection. Clipboard writes affect the Mac running the Rex server.

Run `go test ./cmd/rex-copy-mode` for motions and selection, and `python3 tests/rex-copy-mode.py` for the live Rex integration. The live test starts a private server and checks the entry binding, preserved styles, history, Unicode, wrapped selections, cancellation, snapshot stability, duplicate entry, cleanup, focus restoration, and in-place replacement in split and zoomed layouts. It needs Swift to save and restore all clipboard formats.

The feasibility investigation is in [research/rex-copy-mode.md](research/rex-copy-mode.md).
