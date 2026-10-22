# Rex copy mode

Reload the Rex config with `Ctrl+s`, then `r`. Press `Ctrl+s`, then `[` to open copy mode for the focused terminal.

- Use Vim motions such as `h/j/k/l`, `w/b`, `0/$`, `gg/G`, and `/` to navigate.
- Press Space to start an inclusive selection, then move to its end.
- Press Enter to copy to the macOS clipboard and return to the terminal.
- Press `q` or Escape to cancel. Enter without a selection does nothing.

Copy mode opens an isolated Neovim viewer in a full-size floating layer. It captures the source pane's available screen and scrollback in memory. The source keeps running, while the snapshot stays fixed. Soft-wrapped rows become logical lines, so copying a long line adds no artificial newlines. Trailing spaces are preserved.

Requires Neovim 0.10 or newer and macOS `pbcopy`. Executable lookup includes the dotfiles' `~/.bin` and bob-managed Neovim directory, since a GUI-launched server has a minimal PATH. The viewer loads no user Neovim config or plugins and writes no snapshot, swap, undo, or history files. A clipboard failure keeps the viewer and selection open.

The layer is visible to other clients attached to the session. Clipboard writes affect the Mac running the viewer. Remote-client clipboard delivery is not implemented.

Run `python3 tests/rex-copy-mode.py` to exercise the real Rex and Neovim integration. It starts a private server and checks the entry binding, history, Unicode, wrapped selections, cancellation, clipboard failure, resizing, snapshot stability, duplicate entry, cleanup, and focus restoration. The test requires Swift to save and restore all clipboard formats in memory. It tests the resolved entry binding and invokes the action through the CLI; physical prefix-key dispatch still needs a GUI check.

The feasibility investigation is in [research/rex-copy-mode.md](research/rex-copy-mode.md).
