# Rex copy mode

Reload the Rex config with `Ctrl+s`, then `r`. Press `Ctrl+s`, then `[` to open copy mode for the focused terminal.

- Use Vim motions such as `h/j/k/l`, `w/b`, `0/$`, `gg/G`, and `/` to navigate.
- Press Space to start an inclusive selection, then move to its end.
- Press Enter to copy to the macOS clipboard and return to the terminal.
- Press `q` or Escape to cancel. Enter without a selection does nothing.

Copy mode opens a Neovim viewer in a floating layer over the source pane, or the whole window when the source is zoomed. It captures the source pane's available screen and scrollback in memory. The source keeps running, while the snapshot stays fixed. Soft-wrapped rows become logical lines, so copying a long line adds no artificial newlines. Trailing spaces are preserved.

Requires Neovim 0.10 or newer, the installed editor theme, and macOS `pbcopy`. Executable lookup includes the dotfiles' `~/.bin` and bob-managed Neovim directory, since a GUI-launched server has a minimal PATH. The viewer loads the shared `nvim/lua/theme.lua` module and its theme package, not the main editor config or editing plugins. Change the package and colorscheme there to update both viewers. Appearance is read through `~/.bin/is-dark-theme` at startup and on `SIGUSR1`, rather than relying on Rex's inherited shell environment. Install the theme through the main Neovim config first.

There is no Neovim status line, tab line, gutter, or idle command line. Search and commands still show their prompts. A clipboard failure exposes the message row and keeps the viewer and selection open. The viewer writes no snapshot, swap, undo, or history files.

At equal terminal dimensions, screen rows stay aligned, including soft wraps and short output with blank rows below. Rex still draws a floating-pane header and border, even with normal pane headers disabled. This shifts the text and reduces the available grid, so pixel-perfect alignment needs support from Rex. The snapshot is plain text; terminal ANSI styling is not retained.

The layer is visible to other clients attached to the session. Clipboard writes affect the Mac running the viewer. Remote-client clipboard delivery is not implemented.

Run `python3 tests/rex-copy-mode.py` to exercise the real Rex and Neovim integration. It starts a private server and checks the entry binding, history, Unicode, wrapped selections, cancellation, clipboard failure, resizing, snapshot stability, duplicate entry, cleanup, focus restoration, screen-row alignment at equal dimensions, split-pane bounds, and theme parity with the main editor. The test requires Swift to save and restore all clipboard formats in memory. It tests the resolved entry binding and invokes the action through the CLI; physical prefix-key dispatch still needs a GUI check.

The feasibility investigation is in [research/rex-copy-mode.md](research/rex-copy-mode.md).
