#!/usr/bin/env python3
"""Check the Fish port in a temporary home, without changing the login shell."""

import hashlib
import os
from pathlib import Path
import pty
import select
import shlex
import shutil
import subprocess
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]
ZSH = shutil.which("zsh")
FISH = shutil.which("fish")


class ShellParity(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="zsh-parity-")
        self.addCleanup(self.temp.cleanup)
        self.home = Path(self.temp.name).resolve()
        (self.home / ".config").mkdir()
        (self.home / ".config/zsh").symlink_to(ROOT / ".config/zsh")
        for name in [".zshrc", ".zprofile"]:
            (self.home / name).symlink_to(ROOT / name)
        (self.home / ".ssh").mkdir()
        (self.home / ".ssh/agent.fish").write_text(
            f"set -gx SSH_AUTH_SOCK {self.home}/agent.sock\n"
            f"set -gx SSH_AGENT_PID {os.getpid()}\n"
        )
        self.bin = self.home / "stubs"
        self.bin.mkdir()
        self.env = {
            "HOME": str(self.home),
            "PATH": os.environ["PATH"],
            "TERM": "xterm-256color",
            "SHELL": "/opt/homebrew/bin/fish",
            "TMPDIR": str(self.home) + "/",
        }

    def stub(self, name, body):
        script = self.bin / name
        script.write_text("#!/bin/sh\n" + body + "\n")
        script.chmod(0o755)
        return script

    def run_shell(self, shell, script):
        result = subprocess.run(
            [shell, "-c", script] if shell == FISH else [shell, "-fc", script],
            env=self.env, cwd=self.home, text=True, capture_output=True, timeout=30,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        return result.stdout

    def zsh(self, script):
        return self.run_shell(
            ZSH,
            f'source "{ROOT}/.config/zsh/functions.zsh"; '
            f'source "{ROOT}/.config/zsh/env.zsh"; '
            f'path=("{self.bin}" $path); ' + script,
        )

    def test_syntax(self):
        files = [ROOT / ".zshrc", ROOT / ".zprofile"]
        files += list((ROOT / ".config/zsh").glob("*.zsh"))
        files += list((ROOT / ".config/zsh/per-host").glob("*.zsh"))
        files += list((ROOT / ".config/zsh/func").glob("_*"))
        for file in files:
            with self.subTest(file=file.name):
                result = subprocess.run([ZSH, "-n", str(file)], capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, result.stderr)

    def test_environment_matches_fish(self):
        # Ignore inherited universal paths. Compare what the tracked config sets.
        env_file = ROOT / ".config/fish/conf.d/env.fish"
        names = []
        for line in env_file.read_text().splitlines():
            if line.lstrip().startswith("set -gx "):
                name = line.split()[2]
                if name not in names:
                    names.append(name)
        names += ["PATH", "SSH_AUTH_SOCK", "SSH_AGENT_PID"]
        for dark in [0, 1]:
            fish_script = (
                f'function is-dark-theme; return {1 - dark}; end; '
                'set -g fish_user_paths; '
                f'string join : $PATH; source "{env_file}"; command env'
            )
            zsh_script = (
                f'source "{ROOT}/.config/zsh/functions.zsh"; '
                f'is-dark-theme() {{ return {1 - dark}; }}; '
                f'source "{ROOT}/.config/zsh/env.zsh"; command env'
            )
            fish_output = self.run_shell(FISH, fish_script)
            # System/vendor startup is outside the tracked config. Give Zsh the
            # same starting PATH, then compare the changes made by env files.
            baseline_path = fish_output.splitlines()[0]
            zsh_output = self.run_shell(ZSH, f'PATH={shlex.quote(baseline_path)}; ' + zsh_script)
            snapshots = [
                dict(line.split("=", 1) for line in output.splitlines() if "=" in line)
                for output in [fish_output, zsh_output]
            ]
            for name in names:
                with self.subTest(theme=dark, variable=name):
                    self.assertEqual(snapshots[0].get(name), snapshots[1].get(name))
            self.assertNotIn("CARGO_TARGET_DIR", snapshots[1])

    def test_abbreviations_match_fish(self):
        fish_script = (
            f'source "{ROOT}/.config/fish/conf.d/aliases.fish"; '
            'abbr --show'
        )
        result = subprocess.run(
            [FISH, "--no-config", "-ic", fish_script], env=self.env,
            cwd=self.home, text=True, capture_output=True, timeout=30,
        )
        # Fish can emit terminal warnings without a TTY; only inspect abbr output.
        self.assertEqual(result.returncode, 0, result.stderr)
        fish_abbr = {}
        for line in result.stdout.splitlines():
            if line.startswith("abbr "):
                args = shlex.split(line)
                fish_abbr[args[-2]] = args[-1]
        definitions = self.zsh(
            f'source "{ROOT}/.config/zsh/lightweight-abbr.zsh"; '
            'for key in ${(k)ZSH_ABBREVIATIONS}; do '
            'print -r -- "$key=${ZSH_ABBREVIATIONS[$key]}"; done'
        )
        zsh_abbr = dict(line.split("=", 1) for line in definitions.splitlines() if "=" in line)
        exceptions = {"es": "exec zsh", "sourceenv": "source ./venv/bin/activate"}
        for key, expansion in zsh_abbr.items():
            if key in exceptions:
                self.assertEqual(expansion, exceptions[key])
            else:
                self.assertEqual(expansion, fish_abbr[key], key)
        self.assertEqual(set(zsh_abbr), set(fish_abbr) - {"switch"})
        self.assertEqual(
            self.zsh(f'source "{ROOT}/.config/zsh/lightweight-abbr.zsh"; '
                     'print -r -- "${ZSH_COMMAND_ABBREVIATIONS[wt:switch]}"').strip(),
            fish_abbr["switch"],
        )

    def test_theme_matches_fish(self):
        for dark, mode in [(0, "light"), (1, "dark")]:
            colors = self.run_shell(
                FISH,
                f'fish_config theme choose --color-theme={mode} catppuccin-macchiato; '
                'printf "%s\\n" "$fish_color_command" "$fish_color_param" '
                '"$fish_color_comment" "$fish_color_autosuggestion"',
            ).splitlines()
            styles = self.zsh(
                f'__IS_DARK_THEME={dark}; source "{ROOT}/.config/zsh/theme.zsh"; '
                'printf "%s\\n" "$ZSH_HIGHLIGHT_STYLES[command]" '
                '"$ZSH_HIGHLIGHT_STYLES[default]" "$ZSH_HIGHLIGHT_STYLES[comment]" '
                '"$ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE"'
            ).splitlines()
            self.assertEqual(styles, ["fg=#" + color.split()[0] for color in colors])

    def test_cargo_wrapper(self):
        workspace = self.home / "workspace with spaces"
        workspace.mkdir()
        manifest = workspace / "Cargo.toml"
        manifest.write_text("[workspace]\n")
        self.env["MANIFEST"] = str(manifest)
        self.stub("cargo", """
if [ "$1" = "+nightly" ]; then shift; fi
if [ "$1" = "locate-project" ]; then
    [ "${OUTSIDE:-0}" = 1 ] && exit 1
    printf '%s\\n' "$MANIFEST"
else
    printf 'target=%s\\n' "${CARGO_TARGET_DIR:-}"
    printf 'arg=%s\\n' "$@"
    exit "${CARGO_EXIT:-0}"
fi""")
        target_dir = self.home / ".cargo-target" / hashlib.sha256(str(manifest).encode()).hexdigest()
        for args in ["build", "+nightly check", f'check --manifest-path "{manifest}"', f'check --manifest-path="{manifest}"']:
            output = self.zsh("cargo " + args)
            self.assertIn(f"target={target_dir}", output)
            self.assertEqual((workspace / "target").resolve(), target_dir)
        output = self.zsh("cargo build --target-dir explicit")
        self.assertIn("target=\n", output)
        self.env["OUTSIDE"] = "1"
        self.assertIn("target=\n", self.zsh("cargo new example"))
        del self.env["OUTSIDE"]
        self.env["CARGO_EXIT"] = "7"
        self.assertEqual(self.zsh("cargo build; print $?").splitlines()[-1], "7")
        del self.env["CARGO_EXIT"]
        (workspace / "target").unlink()
        (workspace / "target").mkdir()
        self.assertEqual(self.zsh("cargo build; print $?").strip(), "1")
        self.assertTrue((workspace / "target").is_dir())

    def test_helpers(self):
        self.stub("testsearch", "printf 'one test.py::test_a\\ntwo.py::test_b\\n'")
        self.stub("pytest", "printf '<%s>\\n' \"$@\"")
        self.stub("nono", "printf '%s\\n' \"$NONO_THEME\"; printf '<%s>\\n' \"$@\"")
        self.stub("mise", "printf '<%s>\\n' \"$@\"")
        script = f'source "{ROOT}/.config/zsh/aliases.zsh"; '
        self.assertEqual(self.zsh(script + "ptl"), "<one test.py::test_a>\n<two.py::test_b>\n")
        for dark, theme in [(0, "latte"), (1, "mocha")]:
            self.assertEqual(self.zsh(f'__IS_DARK_THEME={dark}; nono "a b"'), f"{theme}\n<a b>\n")
        self.assertEqual(self.zsh('mcd "new directory"; print "$PWD"').strip(), str(self.home / "new directory"))
        self.assertEqual(self.zsh("pi-update"), f"<--cd>\n<{self.home}>\n<upgrade>\n<npm:@earendil-works/pi-coding-agent>\n")

    def test_interactive_widgets(self):
        pid, fd = pty.fork()
        if pid == 0:
            os.chdir(self.home)
            os.execve(ZSH, [ZSH, "-l"], self.env)
        self.addCleanup(self.stop_shell, pid, fd)

        def send(data):
            os.write(fd, data.encode())

        def wait_for(marker):
            output = b""
            deadline = time.monotonic() + 15
            while time.monotonic() < deadline:
                if select.select([fd], [], [], 0.1)[0]:
                    output += os.read(fd, 65536)
                    # Wait for ZLE, not just the command's output. Sending
                    # input during precmd can lose it when terminal modes reset.
                    marker_at = output.find(marker.encode())
                    if marker_at >= 0 and b"\x1b[?2004h" in output[marker_at:]:
                        return output
            self.fail(f"Terminal did not produce {marker}: {output[-4000:]!r}")

        send("printf '__%s__\\n' READY\n")
        startup = wait_for("__READY__")
        self.assertNotIn(b"can't change option", startup)
        self.assertNotIn(b"no such file", startup)
        send(
            'capture() { printf \'%s\\n\' "$BUFFER" "$POSTDISPLAY" "${(j:,:)region_highlight}" > "$HOME/capture"; '
            "BUFFER=''; zle reset-prompt; }; zle -N capture; bindkey '^Xp' capture; "
            "ZSH_AUTOSUGGEST_IGNORE_WIDGETS+=(capture); "
            "print -s -- 'echo fish-parity-suggestion'; printf '__%s__\\n' BOUND\n"
        )
        wait_for("__BOUND__")

        def capture(line):
            file = self.home / "capture"
            file.unlink(missing_ok=True)
            send(line)
            time.sleep(0.3)
            send("\x18p")
            deadline = time.monotonic() + 5
            while not file.exists() and time.monotonic() < deadline:
                select.select([fd], [], [], 0.05)
                if select.select([fd], [], [], 0)[0]:
                    os.read(fd, 65536)
            self.assertTrue(file.exists(), line)
            return file.read_text().splitlines()

        for line, expected in [
            ("g ", "git "), ("echo g ", "echo g "), ("true; g ", "true; git "),
            ("wt switch ", "wt switch --no-cd "), ("wt --verbose switch ", "wt --verbose switch --no-cd "),
            ("echo wt switch ", "echo wt switch "), ("awslocal ", "aws "),
            ("'g' ", "'g' "), ("AWS_PROFILE=test g ", "AWS_PROFILE=test git "),
        ]:
            with self.subTest(line=line):
                self.assertEqual(capture(line)[0], expected)
        suggestion = capture("echo fish-parity")
        self.assertEqual(suggestion[1], "-suggestion")
        self.assertIn("fg=#", suggestion[2])

        for line, expected in [
            # Codex's generated completer lists first, then inserts on Tab.
            ("codex comple\t\t", "codex completion "),
            ("bob us\t", "bob use "),
            ("jj-hp --hel\t", "jj-hp --help "),
            ("wt --hel\t", "wt --help "),
        ]:
            with self.subTest(completion=line):
                actual = capture(line)[0]
                terminal = b""
                while select.select([fd], [], [], 0)[0]:
                    terminal += os.read(fd, 65536)
                self.assertEqual(actual, expected, terminal[-3000:])

        self.env["SHELL_AGENT_BIN"] = str(self.stub(
            "shell-agent",
            'printf \'%s\\n\' "$@" > "$HOME/agent-args"; '
            'while [ "$#" -gt 0 ] && [ "$1" != --prompt-file ]; do shift; done; shift; '
            'cp "$1" "$HOME/agent-prompt"; rm "$1"',
        ))
        send(f'export SHELL_AGENT_BIN="{self.env["SHELL_AGENT_BIN"]}"; printf \'__%s__\\n\' AGENT\n')
        wait_for("__AGENT__")
        send(": explain 'a b' safely\n")
        deadline = time.monotonic() + 5
        terminal = b""
        while not (self.home / "agent-prompt").exists() and time.monotonic() < deadline:
            if select.select([fd], [], [], 0.05)[0]:
                terminal += os.read(fd, 65536)
        if not (self.home / "agent-prompt").exists():
            args_file = self.home / "agent-args"
            args = args_file.read_text() if args_file.exists() else "runner not invoked"
            self.fail(f"Shell Agent did not run, {args!r}: {terminal[-4000:]!r}")
        self.assertEqual((self.home / "agent-prompt").read_text(), "explain 'a b' safely")
        self.assertEqual((self.home / "agent-args").read_text().splitlines()[:2], ["--pwd", str(self.home)])
        send("bindkey '^R'; bindkey '^T'; bindkey '\\ec'; printf '__%s__\\n' KEYS\n")
        keys = wait_for("__KEYS__")
        for widget in [b"atuin-search", b"fzf-file-widget", b"fzf-cd-widget"]:
            self.assertIn(widget, keys)

    @staticmethod
    def stop_shell(pid, fd):
        # Let history hooks finish before deleting their temporary database.
        os.write(fd, b"\x03exit\n")
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            if os.waitpid(pid, os.WNOHANG)[0]:
                os.close(fd)
                return
            if select.select([fd], [], [], 0.05)[0]:
                try:
                    os.read(fd, 65536)
                except OSError:
                    break
        os.close(fd)
        try:
            os.kill(pid, 15)
        except ProcessLookupError:
            pass
        os.waitpid(pid, 0)


if __name__ == "__main__":
    if not ZSH or not FISH:
        raise SystemExit("Install zsh and fish before running this check.")
    unittest.main()
