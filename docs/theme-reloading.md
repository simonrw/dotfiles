# Automatic theme updates

On macOS, `dark-notify` listens for system appearance changes and invokes `~/.bin/reload-theme`. It also invokes it once at startup. There is no polling loop.

The `reload-theme` LaunchAgent is declared in `mise.macos.toml`. Mise installs and manages `~/Library/LaunchAgents/dev.mise.reload-theme.plist`; do not maintain a separate plist or Homebrew service for it.

## Setup

Install the listener and apply the declared agents from this repository:

```sh
brew install cormacrelf/tap/dark-notify
mise bootstrap macos launchd-agents apply --dry-run
mise bootstrap macos launchd-agents apply --yes
mise bootstrap macos launchd-agents status
```

Review the dry run: this command applies all pending agent definitions, not just the theme listener. Dotfile links must already be installed with `mise bootstrap dotfiles apply`.

Existing zsh panes need a one-time `exec "$SHELL"` at an idle prompt to load the new integration. New panes load it automatically. Normal theme changes do not restart shells.

The agent runs `mise exec` with Python enabled and an explicit Homebrew/system PATH, without relying on an interactive shell's environment. Its output goes to `~/Library/Logs/reload-theme.log`.

## How shells update

Each interactive zsh inside tmux creates a private FIFO and registers its path on the pane. `reload-theme` writes a notification to every registered pane across all sessions on the selected tmux server.

Zsh's line editor watches the FIFO for input. On a notification it reloads only the theme-dependent environment and syntax colours, then repaints the existing input and autosuggestion. It does not inject keystrokes, send process signals, or run `exec`. Unfinished commands, cursor positions, shell variables, and jobs remain intact.

While a command is running, the notification waits in the pipe. Zsh applies it when it returns to the prompt. Closed readers and panes without the integration are skipped. A nested zsh registers its own reader; the parent re-registers when it returns to its prompt.

The existing tmux `prefix+r` binding and manual `reload-theme` command use the same path. Without tmux, the script still updates the other applications. The LaunchAgent targets the default tmux server; invoking `reload-theme` from a named server targets that server instead.

Fish is not changed by this integration. There is no hard-coded shell restart command; only zsh panes that have loaded the reader participate.

## Checks

```sh
python3 tests/reload-theme.py -v
```

The tests use a temporary home and a private tmux server. They cover light/dark transitions across sessions, repainting partially typed input, preserving autosuggestions and cursor position, deferring updates during commands, stale readers, and updates without tmux. They do not switch the real system appearance or signal your editors.
