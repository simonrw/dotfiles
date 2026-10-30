#!/usr/bin/env python3
"""Live copy-mode regression on a private Rex server. Requires macOS, Rex, Neovim and Swift."""

import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
REX = ROOT / ".bin/rex"
FIXTURE = ["old history", *[f"row-{index}" for index in range(100)], "café 界 e\u0301", "wrap:" + "abcdefghij" * 24, "READY"]

# Keep every pasteboard format in memory while the real pbcopy/pbpaste checks run.
CLIPBOARD_GUARD = r'''
import AppKit
let board = NSPasteboard.general
let saved = (board.pasteboardItems ?? []).map { item -> NSPasteboardItem in
    let copy = NSPasteboardItem()
    for type in item.types {
        guard let data = item.data(forType: type) else { fatalError("Cannot preserve clipboard format") }
        copy.setData(data, forType: type)
    }
    return copy
}
let child = Process()
child.executableURL = URL(fileURLWithPath: CommandLine.arguments[1])
child.arguments = Array(CommandLine.arguments.dropFirst(2))
child.standardOutput = FileHandle.standardOutput
child.standardError = FileHandle.standardError
do {
    try child.run()
    child.waitUntilExit()
} catch {
    fputs("Could not start tests: \(error)\n", stderr)
    exit(1)
}
board.clearContents()
if !saved.isEmpty && !board.writeObjects(saved) { fatalError("Cannot restore clipboard") }
exit(child.terminationStatus)
'''


class CopyMode(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp = tempfile.TemporaryDirectory(prefix="rex-copy-test-", dir="/tmp")
        cls.addClassCleanup(cls.temp.cleanup)
        cls.directory = Path(cls.temp.name)
        cls.server_address = f"unix://{cls.directory}/server.sock"
        cls.env = {name: value for name, value in os.environ.items() if not name.startswith("REX_")}
        # Match the GUI server's minimal PATH rather than the developer's interactive shell.
        cls.env["PATH"] = "/usr/bin:/bin:/usr/sbin:/sbin"
        cls.env["XDG_CONFIG_HOME"] = str(ROOT / ".config")
        cls.env["REX_SERVER"] = cls.server_address
        cls.server_log = (cls.directory / "server.log").open("w+")
        cls.addClassCleanup(cls.server_log.close)
        cls.server = subprocess.Popen(
            [str(REX), "server", "run", "--listen", cls.server_address,
             "--state-dir", str(cls.directory / "state"), "--config", str(ROOT / ".config/rex/init.lua")],
            env=cls.env, stdout=cls.server_log, stderr=cls.server_log,
        )
        cls.addClassCleanup(cls.stop_server)
        deadline = time.monotonic() + 10
        while time.monotonic() < deadline:
            if (cls.directory / "server.sock").exists():
                return
            if cls.server.poll() is not None:
                break
            time.sleep(0.05)
        cls.server_log.seek(0)
        raise AssertionError(f"Test server did not start: {cls.server_log.read()}")

    @classmethod
    def stop_server(cls):
        cls.server.terminate()
        try:
            cls.server.wait(timeout=5)
        except subprocess.TimeoutExpired:
            cls.server.kill()
            cls.server.wait()

    def rex(self, *args):
        return subprocess.check_output(
            [str(REX), "--autostart=false", "--server", self.server_address, *args],
            env=self.env, text=True, timeout=15,
        )

    def api(self, method, **args):
        return json.loads(self.rex("api", "call", method, json.dumps(args)))

    def wait_for(self, description, check):
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            result = check()
            if result:
                return result
            time.sleep(0.05)
        self.fail(f"Timed out waiting for {description}")

    def setUp(self):
        # The source stays alive and records input, so leaked viewer keys are observable.
        self.input_log = self.directory / "source-input"
        self.input_log.write_bytes(b"")
        source = "import os,sys,tty; tty.setraw(0); print(" + repr("\r\n".join(FIXTURE)) + ",end='',flush=True); "
        source += "log=open(sys.argv[1],'ab',buffering=0)\nwhile True: log.write(os.read(0,1024))"
        created = json.loads(self.rex(
            "new", "copy-test", "--json", "--shell", "none", "--",
            sys.executable, "-c", source, str(self.input_log),
        ))
        self.session = created["session_id"]
        self.source = created["initial_windows"][0]["block_ids"][0]
        self.addCleanup(self.rex, "kill", self.session)
        self.wait_for("source output", lambda: "READY" in self.rex("capture", "--block", self.source))

    def open_viewer(self):
        viewer = json.loads(self.rex("do", "copy_mode", "--session", self.session, "--block", self.source))
        block = viewer["block_ids"][0]
        self.wait_for("copy viewer", lambda: (
            self.api("com.superlogical.terminal.modes", session_id=self.session, block_id=block, args={})["screen"] == "alternate"
            and "READY" in self.rex("capture", "--block", block)
        ))
        return block

    def keys(self, block, *keys):
        self.rex("send-key", "--session", self.session, "--block", block, *keys)

    def wait_closed(self):
        def closed():
            window = self.api("session.view", session_id=self.session)["windows"][0]
            return len(window["layers"]) == 1 and window["focused_block_id"] == self.source
        self.wait_for("viewer close and source focus", closed)

    def clipboard(self):
        return subprocess.check_output(["/usr/bin/pbpaste"])

    def screen(self, block):
        size = self.api("com.superlogical.terminal.size", session_id=self.session, block_id=block, args={})
        content = self.api("com.superlogical.terminal.format", session_id=self.session, block_id=block,
                           args={"format": "text", "trim": False, "unwrap": False})["content"]
        lines = content.split("\n")[-size["rows"]:]
        return [line.ljust(size["columns"]) for line in lines] + [" " * size["columns"]] * (size["rows"] - len(lines))

    def test_viewer_preserves_screen_rows(self):
        before = self.screen(self.source)
        viewer = self.open_viewer()
        self.assertEqual(self.screen(viewer), before)

    def test_viewer_preserves_short_screen_and_blank_rows(self):
        # A fresh terminal renders a short fixture at the top, with empty rows below.
        created = json.loads(self.rex(
            "run", "--session", self.session, "--json", "--shell", "none", "--",
            sys.executable, "-c", "import time; print('short café 界\\r\\nREADY', end='', flush=True); time.sleep(60)",
        ))
        self.source = created["block_ids"][0]
        self.wait_for("short fixture", lambda: "READY" in self.rex("capture", "--block", self.source))
        before = self.screen(self.source)
        viewer = self.open_viewer()
        self.assertEqual(self.screen(viewer), before)

    def test_viewer_covers_only_source_pane(self):
        self.rex("split", "--session", self.session, "--block", self.source,
                 "--split", "right", "--ratio", "35", "--focus=false", "--shell", "none", "--", "/bin/cat")
        window = self.api("session.view", session_id=self.session)["windows"][0]
        expected = window["layers"][0]["layout"]["split"]["before"]["rect"]
        for zoomed in [False, True]:
            with self.subTest(zoomed=zoomed):
                if zoomed:
                    self.rex("zoom", "--session", self.session, self.source, "--on")
                viewer = self.open_viewer()
                window = self.api("session.view", session_id=self.session)["windows"][0]
                self.assertEqual(window["layers"][-1]["bounds"], [0, 0, 1, 1] if zoomed else expected)
                self.keys(viewer, "q")
                self.wait_closed()

    def test_viewer_matches_editor_theme_and_reloads_appearance(self):
        theme = "{name=vim.g.colors_name, background=vim.o.background, normal=vim.api.nvim_get_hl(0, {name='Normal', link=false})}"
        expected_path = self.directory / "editor-theme.json"
        actual_path = self.directory / "viewer-theme.json"
        expected_path.unlink(missing_ok=True)
        actual_path.unlink(missing_ok=True)
        nvim = shutil.which("nvim") or str(Path.home() / ".local/share/bob/nvim-bin/nvim")
        subprocess.run(
            [nvim, "--headless", "-n", "-i", "NONE", "-c",
             f"lua vim.fn.writefile({{vim.json.encode({theme})}}, {json.dumps(str(expected_path))})", "-c", "qa!"],
            env={**os.environ, "XDG_CONFIG_HOME": str(ROOT / ".config"), "TMPDIR": str(self.directory)},
            check=True, capture_output=True, timeout=30,
        )
        expected = json.loads(expected_path.read_text())
        viewer = self.open_viewer()

        def read_theme():
            actual_path.unlink(missing_ok=True)
            self.rex("send", "--session", self.session, "--block", viewer,
                     f":lua vim.fn.writefile({{vim.json.encode({theme})}}, {json.dumps(str(actual_path))})\r")
            self.wait_for("viewer theme", actual_path.exists)
            return json.loads(actual_path.read_text())

        initial = read_theme()
        # A theme notification must read current appearance, not a stale inherited variable.
        self.rex("send", "--session", self.session, "--block", viewer,
                 ":lua vim.env.__IS_DARK_THEME = '" + ("1" if expected["background"] == "light" else "0")
                 + "'; vim.api.nvim_exec_autocmds('Signal', {pattern='SIGUSR1'})\r")
        reloaded = read_theme()
        self.assertEqual([initial, reloaded], [expected, expected])

    def test_entry_binding_and_cancel_do_not_leak_input(self):
        keymap = json.loads(self.rex("keymap", "--json"))
        self.assertTrue(any(binding.get("action") == "copy_mode" and binding["keys"] == ["ctrl+s", "["]
                            for binding in keymap["bindings"]))
        before = self.clipboard()
        viewer = self.open_viewer()
        self.keys(viewer, "enter", "g", "g", "space", "j", "escape")
        self.wait_closed()
        self.assertEqual((self.clipboard(), self.input_log.read_bytes()), (before, b""))

    def test_copy_history_and_restore_focus(self):
        viewer = self.open_viewer()
        self.keys(viewer, "g", "g", "space", "shift+4", "enter")
        self.wait_closed()
        self.assertEqual(self.clipboard(), b"old history")

    def test_reversed_unicode_selection(self):
        viewer = self.open_viewer()
        self.keys(viewer, "g", "g", "1", "0", "2", "g", "g", "shift+4", "space", "0", "enter")
        self.wait_closed()
        self.assertEqual(self.clipboard(), FIXTURE[101].encode())

    def test_wrapped_multiline_selection(self):
        viewer = self.open_viewer()
        self.keys(viewer, "g", "g", "1", "0", "3", "g", "g", "0", "space", "j", "shift+4", "enter")
        self.wait_closed()
        self.assertEqual(self.clipboard(), "\n".join(FIXTURE[102:]).encode())

    def test_reentry_reuses_viewer_and_snapshot_stays_frozen(self):
        viewer = self.open_viewer()
        repeated = json.loads(self.rex("do", "copy_mode", "--session", self.session, "--block", self.source))
        self.assertEqual(repeated["block_ids"], [viewer])
        self.api("com.superlogical.terminal.reset", session_id=self.session, block_id=self.source, args={})
        self.api("com.superlogical.terminal.resize", session_id=self.session, block_id=viewer,
                 args={"columns": 40, "rows": 12})
        self.keys(viewer, "g", "g", "space", "shift+4", "enter")
        self.wait_closed()
        self.assertEqual(self.clipboard(), b"old history")

    def test_clipboard_failure_keeps_viewer_open(self):
        before = self.clipboard()
        viewer = self.open_viewer()
        # Remove pbcopy from this viewer's PATH through Neovim's public command interface.
        self.rex("send", "--session", self.session, "--block", viewer, ":let $PATH = ''\r")
        self.keys(viewer, "g", "g", "space", "l", "enter")
        self.wait_for("clipboard error", lambda: "Could not copy" in self.rex("capture", "--block", viewer))
        blocks = self.api("session.list_blocks", session_id=self.session)["blocks"]
        self.assertIn(viewer, [block["block_id"] for block in blocks])
        self.assertEqual(self.clipboard(), before)


if __name__ == "__main__":
    if sys.platform != "darwin":
        raise SystemExit("These live tests require macOS clipboard tools.")
    if "--inside-clipboard-guard" not in sys.argv:
        raise SystemExit(subprocess.run(
            ["/usr/bin/swift", "-", sys.executable, str(Path(__file__).resolve()), "--inside-clipboard-guard"],
            input=CLIPBOARD_GUARD, text=True, check=False,
        ).returncode)
    sys.argv.remove("--inside-clipboard-guard")
    unittest.main()
