#!/usr/bin/env python3
"""Exercise session tools with a stateful Rex CLI stub, without switching sessions."""

import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]

STUB = r'''#!/usr/bin/env python3
import json
import os
from pathlib import Path
import sys

name = Path(sys.argv[0]).name
args = sys.argv[1:]
with open(os.environ["TEST_LOG"], "a") as log:
    log.write(json.dumps([name, *args]) + "\n")
if name == "rex":
    if args[0] == os.getenv("TEST_FAIL"):
        sys.exit(7)
    state = Path(os.environ["TEST_STATE"])
    sessions = json.loads(state.read_text())
    if args == ["ls", "--json"]:
        print(json.dumps({"sessions": sessions}))
    elif args[0] == "new":
        assert args[2] == "--cwd" and args[4] == "--json", args
        session = {"session_id": "session:new", "label": args[1]}
        sessions.append(session)
        state.write_text(json.dumps(sessions))
        print(json.dumps({"session_id": session["session_id"], "initial_windows": []}))
    elif args[0] == "attach":
        assert any(s["session_id"] == args[1] for s in sessions), args
    elif args[:2] == ["do", "session.select"]:
        assert any("session_id=" + s["session_id"] == args[2] for s in sessions), args
    elif args[0] == "kill":
        assert any(s["session_id"] == args[1] for s in sessions), args
        state.write_text(json.dumps([s for s in sessions if s["session_id"] != args[1]]))
    else:
        raise AssertionError(args)
elif name == "is-dark-theme":
    sys.exit(1)
elif name == "fzf":
    Path(os.environ["TEST_INPUT"]).write_text(sys.stdin.read())
    if os.getenv("TEST_CANCEL"):
        sys.exit(130)
    print(os.getenv("TEST_CHOICE", ""))
elif name == "tmux":
    if args == ["ls", "-F", "#S"]:
        sys.exit(1)
'''


class RexSessions(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.build = tempfile.TemporaryDirectory(prefix="rex-session-build-")
        cls.addClassCleanup(cls.build.cleanup)
        cls.binary = Path(cls.build.name) / "mux-session-name"
        subprocess.run(
            ["go", "build", "-o", str(cls.binary), "./cmd/mux-session-name"],
            cwd=ROOT, check=True, capture_output=True, text=True,
        )

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="rex-session-test-")
        self.addCleanup(self.temp.cleanup)
        self.home = Path(self.temp.name).resolve()
        self.bin = self.home / "bin"
        self.bin.mkdir()
        stub = self.bin / "stub"
        stub.write_text(STUB.replace("#!/usr/bin/env python3", f"#!{sys.executable}", 1))
        stub.chmod(0o755)
        for name in ["rex", "tmux", "herdr", "tat", "is-dark-theme", "fzf"]:
            (self.bin / name).symlink_to(stub)
        (self.bin / "mux-session-name").symlink_to(self.binary)
        self.state = self.home / "sessions.json"
        self.state.write_text("[]")
        self.log = self.home / "commands.jsonl"
        self.log.touch()
        self.env = dict(os.environ)
        for name in ["TMUX", "HERDR_ENV", "REX_SESSION", "REX_BLOCK", "REX_SERVER"]:
            self.env.pop(name, None)
        self.env.update(
            PATH=f"{self.bin}:{self.env['PATH']}", SESSION_BACKEND="rex",
            TEST_LOG=str(self.log), TEST_STATE=str(self.state),
            TEST_INPUT=str(self.home / "picker-input"),
        )
        self.worktree = self.home / "project-feature.with: spaces"

    def run_tool(self, tool, *args, success=True):
        command = [str(ROOT / ".bin" / tool), *map(str, args)]
        result = subprocess.run(
            command, cwd=self.home, env=self.env, capture_output=True, text=True, timeout=10,
        )
        if success:
            self.assertEqual(result.returncode, 0, result.stderr)
        else:
            self.assertNotEqual(result.returncode, 0, result.stdout)
        return result

    def commands(self):
        return [json.loads(line) for line in self.log.read_text().splitlines()]

    def seed(self, *sessions):
        self.state.write_text(json.dumps(list(sessions)))

    def test_create_reuse_and_remove(self):
        self.run_tool("tnew", "--path", self.worktree)
        self.assertTrue(self.worktree.is_dir())
        self.assertEqual(self.commands(), [
            ["rex", "ls", "--json"],
            ["rex", "new", self.worktree.name, "--cwd", str(self.worktree), "--json"],
            ["rex", "attach", "session:new"],
        ])
        self.log.write_text("")
        self.run_tool("tnew", "--path", self.worktree)
        self.run_tool("worktrunk-session-hook", "pre-start", self.worktree)
        self.assertEqual(self.commands(), [
            ["rex", "ls", "--json"], ["rex", "attach", "session:new"],
            ["rex", "ls", "--json"], ["rex", "attach", "session:new"],
        ])
        self.worktree.rmdir()
        self.run_tool("worktrunk-session-hook", "post-remove", self.worktree)
        self.run_tool("worktrunk-session-hook", "post-remove", self.worktree)
        self.assertEqual(json.loads(self.state.read_text()), [])
        self.assertEqual(self.commands().count(["rex", "kill", "session:new"]), 1)

    def test_hook_create_and_exact_label_removal(self):
        unrelated = {"session_id": "session:unrelated", "label": self.worktree.name + "-other"}
        self.seed(unrelated)
        self.run_tool("worktrunk-session-hook", "pre-start", self.worktree)
        self.assertIn(
            ["rex", "new", self.worktree.name, "--cwd", str(self.worktree), "--json"],
            self.commands(),
        )
        self.seed(unrelated,
                  {"session_id": "session:one", "label": self.worktree.name},
                  {"session_id": "session:two", "label": self.worktree.name})
        self.run_tool("worktrunk-session-hook", "post-remove", self.worktree)
        self.assertEqual(json.loads(self.state.read_text()), [unrelated])

    def test_rex_terminal_detection(self):
        self.env["SESSION_BACKEND"] = "tmux"
        self.env["REX_SESSION"] = "session:current"
        self.run_tool("tnew", "--path", self.worktree)
        self.assertEqual(self.commands()[-1], ["rex", "do", "session.select", "session_id=session:new"])
        self.env["SESSION_BACKEND"] = "rex"
        self.run_tool("worktrunk-session-hook", "pre-start", self.worktree)
        self.assertEqual(self.commands()[-1], ["rex", "do", "session.select", "session_id=session:new"])
        self.env.pop("SESSION_BACKEND")
        self.env["TEST_CHOICE"] = f"{self.worktree.name}\tsession:new"
        self.run_tool("tmux-session-history")
        self.assertEqual(self.commands()[-1], ["rex", "do", "session.select", "session_id=session:new"])

    def test_nested_backends_keep_precedence(self):
        for marker, value, backend in [("TMUX", "/tmp/tmux", "tmux"), ("HERDR_ENV", "1", "herdr")]:
            with self.subTest(backend=backend):
                self.env["REX_SESSION"] = "session:outer"
                self.env[marker] = value
                self.log.write_text("")
                self.run_tool("tnew", "--path", self.worktree)
                self.assertTrue(self.commands())
                self.assertTrue(all(cmd[0] == backend for cmd in self.commands()))
                self.env.pop(marker)

    def test_picker_ids_cancellation_and_empty_list(self):
        self.seed({"session_id": "session:chosen", "label": self.worktree.name})
        self.env["TEST_CHOICE"] = f"{self.worktree.name}\tsession:chosen"
        self.run_tool("tmux-session-history")
        self.assertEqual((self.home / "picker-input").read_text(), self.env["TEST_CHOICE"] + "\n")
        self.assertEqual(self.commands()[-1], ["rex", "attach", "session:chosen"])
        for cancel in [True, False]:
            with self.subTest(cancel=cancel):
                self.log.write_text("")
                if cancel:
                    self.env["TEST_CANCEL"] = "1"
                else:
                    self.env.pop("TEST_CANCEL")
                    self.env.pop("TEST_CHOICE")
                    self.seed()
                self.run_tool("tmux-session-history")
                self.assertFalse(any(cmd[:2] == ["rex", "attach"] for cmd in self.commands()))

    def test_server_errors_do_not_create_or_attach(self):
        self.env["TEST_FAIL"] = "ls"
        for tool, args in [
            ("tnew", ["--path", self.worktree]),
            ("worktrunk-session-hook", ["pre-start", self.worktree]),
            ("worktrunk-session-hook", ["post-remove", self.worktree]),
        ]:
            with self.subTest(tool=tool, args=args):
                self.log.write_text("")
                self.run_tool(tool, *args, success=False)
                self.assertEqual(self.commands(), [["rex", "ls", "--json"]])

    def test_create_failure_does_not_attach(self):
        self.env["TEST_FAIL"] = "new"
        for tool, args in [
            ("tnew", ["--path", self.worktree]),
            ("worktrunk-session-hook", ["pre-start", self.worktree]),
        ]:
            with self.subTest(tool=tool):
                self.log.write_text("")
                self.run_tool(tool, *args, success=False)
                self.assertFalse(any(cmd[:2] == ["rex", "attach"] for cmd in self.commands()))


if __name__ == "__main__":
    unittest.main()
