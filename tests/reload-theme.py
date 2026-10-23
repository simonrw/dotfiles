#!/usr/bin/env python3
"""Exercise real zsh readers and reload-theme in an isolated tmux server/home."""

import os
import shlex
import shutil
import subprocess
import tempfile
import time
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TMUX = shutil.which("tmux")
ZSH = shutil.which("zsh")


@unittest.skipUnless(TMUX and ZSH, "Requires tmux and zsh")
class ThemeReload(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="theme-test-")
        self.addCleanup(self.temp.cleanup)
        self.home = Path(self.temp.name).resolve()
        self.socket = str(self.home / "tmux.sock")
        self.env = dict(
            os.environ,
            HOME=str(self.home),
            SHELL=ZSH,
            ZDOTDIR=str(self.home),
            TMPDIR=str(self.home),
            TERM="xterm-256color",
        )
        self.env.pop("TMUX", None)
        self.env.pop("TMUX_PANE", None)
        config = self.home / ".config"
        config.mkdir()
        (config / "zsh").symlink_to(ROOT / ".config/zsh")
        for folder in ["tmux", "helix/themes", "git", "gh-dash"]:
            (config / folder).mkdir(parents=True)
        (config / "tmux/tmux.conf").write_text("set -g @theme_test reloaded\n")
        for name in ["base", "light", "dark"]:
            (config / f"gh-dash/config-{name}.yml").write_text(f"# {name}\n")
        (self.home / "mode").write_text("light")
        # Never signal the developer's editors during an integration test.
        bin_dir = self.home / "bin"
        bin_dir.mkdir()
        pgrep = bin_dir / "pgrep"
        pgrep.write_text("#!/bin/sh\nexit 1\n")
        pgrep.chmod(0o755)
        self.env["PATH"] = str(bin_dir) + os.pathsep + self.env["PATH"]
        (self.home / ".zshrc").write_text("""
is-dark-theme() { [[ $(<"$HOME/mode") == dark ]]; }
source "$HOME/.config/zsh/theme-env.zsh"
source "$HOME/.config/zsh/theme.zsh"
source "$HOME/.config/zsh/prompt.zsh"
source "$HOME/.config/zsh/theme-reload.zsh"
__snapshot() {
    printf '%s\\n' "$BUFFER" "$CURSOR" "$POSTDISPLAY" "$__IS_DARK_THEME" \
        "$FZF_DEFAULT_OPTS" "$BAT_CONFIG_PATH" "$ZSH_HIGHLIGHT_STYLES[command]" \
        "$ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE" "${(j:,:)region_highlight}" \
        > "$HOME/snapshot-${TMUX_PANE#%}"
}
zle -N __snapshot
bindkey '^Xp' __snapshot
source "$HOME/.config/zsh/plugins.zsh"
print -s -- 'echo unfinished suggestion'
""")
        self.addCleanup(self.stop_server)

    def tmux(self, *args, check=True):
        return subprocess.run(
            [TMUX, "-S", self.socket, *args],
            env=self.env,
            capture_output=True,
            text=True,
            check=check,
            timeout=10,
        )

    def stop_server(self):
        # Let zshexit remove the test readers before killing the server.
        panes = self.tmux("list-panes", "-a", "-F", "#{pane_id}", check=False)
        for pane in panes.stdout.splitlines():
            self.tmux(
                "send-keys", "-t", pane, "C-c", "C-u", "exit", "Enter", check=False
            )
        time.sleep(0.1)
        self.tmux("kill-server", check=False)

    def wait_for(self, predicate):
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            result = predicate()
            if result:
                return result
            time.sleep(0.03)
        panes = self.tmux("capture-pane", "-p", check=False).stdout
        self.fail(f"Timed out waiting for shell: {panes}")

    def new_shell(self, name, standalone=False):
        pane = self.tmux(
            "-f",
            "/dev/null",
            "new-session",
            "-d",
            "-s",
            name,
            "-x",
            "100",
            "-y",
            "30",
            "-P",
            "-F",
            "#{pane_id}",
            f"exec {'env -u TMUX ' if standalone else ''}{shlex.quote(ZSH)} -d",
        ).stdout.strip()
        if not standalone:
            self.wait_for(
                lambda: self.tmux(
                    "show-option", "-pqv", "-t", pane, "@theme_reload_fifo"
                ).stdout.strip()
            )
        self.wait_for(lambda: "$" in self.tmux("capture-pane", "-p", "-t", pane).stdout)
        return pane

    def snapshot(self, pane):
        path = self.home / f"snapshot-{pane[1:]}"
        path.unlink(missing_ok=True)
        self.tmux("send-keys", "-t", pane, "C-x", "p")
        return self.wait_for(
            lambda: path.read_text().splitlines() if path.exists() else None
        )

    def reload(self, mode, server=True):
        (self.home / "mode").write_text(mode)
        env = dict(self.env)
        if server:
            pid = self.tmux("display-message", "-p", "#{pid}").stdout.strip()
            env["TMUX"] = f"{self.socket},{pid},0"
        else:
            env["TMUX"] = f"{self.home}/absent.sock,1,0"
        result = subprocess.run(
            [str(ROOT / ".bin/reload-theme"), mode],
            env=env,
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_refreshes_all_sessions_without_changing_input_or_busy_commands(self):
        idle = self.new_shell("idle")
        busy = self.new_shell("busy")
        self.tmux("send-keys", "-t", idle, "-l", "echo unfinished")
        self.tmux("send-keys", "-t", idle, "Left", "Left")
        before = self.wait_for(lambda: s if (s := self.snapshot(idle))[2] else None)
        # Keep the second shell busy until the test explicitly releases it.
        self.tmux(
            "send-keys",
            "-t",
            busy,
            "-l",
            'touch "$HOME/running"; while [[ ! -e "$HOME/release" ]]; do sleep 0.05; done; '
            'printf "%s" "$__IS_DARK_THEME" > "$HOME/during-command"',
        )
        self.tmux("send-keys", "-t", busy, "Enter")
        self.wait_for(lambda: (self.home / "running").exists())
        self.reload("dark")
        after = self.wait_for(
            lambda: s if (s := self.snapshot(idle))[3] == "1" else None
        )
        self.assertEqual(
            after[:3], before[:3], "Input, cursor and suggestion must survive"
        )
        self.assertEqual(after[6], "fg=#8aadf4")
        self.assertIn("fg=#8aadf4", after[8], "Existing input must be repainted")
        self.assertIn("--color dark", after[4])
        self.assertTrue(after[5].endswith("/bat/dark/config"))
        self.assertFalse((self.home / "during-command").exists())
        (self.home / "release").touch()
        self.wait_for(lambda: (self.home / "during-command").exists())
        self.assertEqual((self.home / "during-command").read_text(), "0")
        self.wait_for(lambda: self.snapshot(busy)[3] == "1")
        self.reload("light")
        light = self.wait_for(
            lambda: s if (s := self.snapshot(idle))[3] == "0" else None
        )
        self.assertEqual(light[:3], before[:3])
        self.assertIn("fg=#1e66f5", light[8])
        self.wait_for(lambda: self.snapshot(busy)[3] == "0")
        self.assertEqual(
            self.tmux("show-option", "-gqv", "@theme_test").stdout.strip(), "reloaded"
        )

    def test_parent_catches_up_after_nested_shell_exits(self):
        pane = self.new_shell("nested")
        parent = self.tmux(
            "show-option", "-pqv", "-t", pane, "@theme_reload_fifo"
        ).stdout
        self.tmux("send-keys", "-t", pane, "-l", f"{shlex.quote(ZSH)} -d")
        self.tmux("send-keys", "-t", pane, "Enter")
        self.wait_for(
            lambda: (
                self.tmux(
                    "show-option", "-pqv", "-t", pane, "@theme_reload_fifo"
                ).stdout
                != parent
            )
        )
        self.reload("dark")
        self.wait_for(lambda: self.snapshot(pane)[3] == "1")
        self.tmux("send-keys", "-t", pane, "exit", "Enter")
        self.wait_for(
            lambda: (
                self.tmux(
                    "show-option", "-pqv", "-t", pane, "@theme_reload_fifo"
                ).stdout
                == parent
            )
        )
        self.wait_for(lambda: self.snapshot(pane)[3] == "1")

    def test_shell_outside_tmux_updates_without_a_tmux_server(self):
        pane = self.new_shell("standalone", standalone=True)
        self.tmux("send-keys", "-t", pane, "-l", "echo unfinished")
        self.tmux("send-keys", "-t", pane, "Left", "Left")
        before = self.wait_for(lambda: s if (s := self.snapshot(pane))[2] else None)
        stale = self.home / ".cache/zsh-theme/reader.stale"
        stale.mkdir()
        os.mkfifo(stale / "notify")
        for mode, expected, colour in [
            ["dark", "1", "fg=#8aadf4"],
            ["light", "0", "fg=#1e66f5"],
        ]:
            self.reload(mode, server=False)
            after = self.wait_for(
                lambda: s if (s := self.snapshot(pane))[3] == expected else None
            )
            self.assertEqual(after[:3], before[:3])
            self.assertIn(colour, after[8])
            self.assertIn(f"--color {mode}", after[4])
            self.assertTrue(after[5].endswith(f"/bat/{mode}/config"))

    def test_no_tmux_still_updates_other_apps(self):
        self.reload("dark", server=False)
        self.assertIn(
            "catppuccin_macchiato",
            (self.home / ".config/helix/themes/adaptive.toml").read_text(),
        )
        self.assertIn("dark = true", (self.home / ".config/git/theme.conf").read_text())
        self.assertIn("# dark", (self.home / ".config/gh-dash/config.yml").read_text())

    def test_stale_and_unregistered_panes_are_ignored(self):
        pane = self.new_shell("stale")
        stale = self.home / "stale-fifo"
        os.mkfifo(stale)
        self.tmux("set-option", "-p", "-t", pane, "@theme_reload_fifo", str(stale))
        self.reload("dark")  # Must not block opening a FIFO without a reader.
        self.assertEqual(self.snapshot(pane)[3], "0")
        self.tmux("set-option", "-pu", "-t", pane, "@theme_reload_fifo")
        self.reload("dark")
        self.assertEqual(self.snapshot(pane)[3], "0")


if __name__ == "__main__":
    unittest.main()
