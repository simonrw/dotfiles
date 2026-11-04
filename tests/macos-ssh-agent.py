#!/usr/bin/env python3
"""Exercise socket publication and shell startup with real, isolated SSH agents."""

from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]


@unittest.skipUnless(sys.platform == "darwin", "macOS agent integration")
class MacOSAgent(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="ssh-test-", dir="/tmp")
        self.addCleanup(self.temp.cleanup)
        self.home = Path(self.temp.name).resolve()
        (self.home / ".zshenv").symlink_to(ROOT / ".zshenv")
        self.env = {"HOME": str(self.home), "PATH": "/usr/bin:/bin"}
        self.stable = self.home / ".ssh/macos-agent.sock"
        self.stable.parent.mkdir(mode=0o700)

    def start_agent(self, name):
        socket = self.home / name
        agent = subprocess.Popen(
            ["/usr/bin/ssh-agent", "-D", "-a", str(socket)],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
        )
        self.addCleanup(lambda: (agent.terminate(), agent.wait(timeout=5)))
        deadline = time.monotonic() + 5
        while not socket.is_socket():
            if agent.poll() is not None or time.monotonic() > deadline:
                self.fail("Isolated SSH agent failed to start")
            time.sleep(0.01)
        return socket

    def publish(self, socket, check=True):
        return subprocess.run(
            ["/bin/sh", str(ROOT / ".bin/publish-macos-ssh-agent")],
            env={**self.env, "SSH_AUTH_SOCK": str(socket)},
            capture_output=True, text=True, check=check,
        )

    def test_shells_share_agent_and_survive_republication(self):
        first = self.start_agent("first.sock")
        self.publish(first)
        # Loading an ephemeral test key proves both paths reach the same agent.
        key = self.home / "test-key"
        subprocess.run(
            ["/usr/bin/ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-f", str(key)],
            check=True,
        )
        subprocess.run(
            ["/usr/bin/ssh-add", str(key)],
            env={**self.env, "SSH_AUTH_SOCK": str(first)},
            check=True, capture_output=True,
        )
        for flags in ["-c", "-lc", "-ic"]:
            with self.subTest(shell=flags):
                result = subprocess.run(
                    ["/bin/zsh", flags, '/usr/bin/ssh-add -T "$HOME/test-key.pub"'],
                    env={**self.env, "SSH_AUTH_SOCK": "/stale/tmux.sock", "SSH_AGENT_PID": "123"},
                    capture_output=True, text=True,
                )
                self.assertEqual(result.returncode, 0, result.stderr)

        self.publish(first)  # Repeated publication is harmless.
        second = self.start_agent("second.sock")
        self.publish(second)
        # An existing process keeps the stable path, now reaching the empty agent.
        result = subprocess.run(
            ["/usr/bin/ssh-add", "-l"],
            env={**self.env, "SSH_AUTH_SOCK": str(self.stable)},
            capture_output=True, text=True,
        )
        self.assertEqual(result.returncode, 1, result.stderr)

    def test_invalid_publication_preserves_existing_link(self):
        socket = self.start_agent("agent.sock")
        self.publish(socket)
        for invalid in [self.home / "missing.sock", self.stable]:
            with self.subTest(socket=invalid):
                self.assertNotEqual(self.publish(invalid, check=False).returncode, 0)
                self.assertEqual(self.stable.resolve(), socket)

    def test_publisher_does_not_overwrite_user_file(self):
        socket = self.start_agent("agent.sock")
        self.stable.write_text("keep me")
        self.assertNotEqual(self.publish(socket, check=False).returncode, 0)
        self.assertEqual(self.stable.read_text(), "keep me")


if __name__ == "__main__":
    unittest.main()
