"""Run with: python3 .bin/test_mux_bsp_split.py"""

import json
import os
from pathlib import Path
import runpy
import unittest
from unittest.mock import patch

SCRIPT = Path(__file__).with_name("mux-bsp-split")
MODULE = runpy.run_path(str(SCRIPT))


def rect(x, y, width, height):
    return dict(x=x, y=y, width=width, height=height)


class BspSplitTest(unittest.TestCase):
    def test_tmux_directions(self):
        direction = MODULE["tmux_direction"]
        self.assertEqual(direction("abcd,160x50,0,0,1", "%1"), "-h")
        layout = "abcd,160x50,0,0{80x50,0,0,1,79x50,81,0[79x25,81,0,2,79x24,81,26,3]}"
        self.assertEqual(direction(layout, "%1"), "-v")
        self.assertEqual(direction(layout, "%2"), "-h")
        self.assertEqual(direction(layout, "3"), "-h")
        with self.assertRaises(ValueError):
            direction(layout, "%99")

    def test_herdr_directions(self):
        direction = MODULE["herdr_direction"]
        layout = {"panes": [{"pane_id": "w1:p1", "rect": rect(0, 0, 160, 50)}], "splits": []}
        self.assertEqual(direction(layout, "w1:p1"), "right")
        layout = {
            "panes": [
                {"pane_id": "w1:p1", "rect": rect(0, 0, 80, 50)},
                {"pane_id": "w1:p2", "rect": rect(81, 0, 79, 25)},
                {"pane_id": "w1:p3", "rect": rect(81, 26, 79, 24)},
            ],
            "splits": [
                {"direction": "down", "rect": rect(81, 0, 79, 50)},
                {"direction": "right", "rect": rect(0, 0, 160, 50)},
            ],
        }
        self.assertEqual(direction(layout, "w1:p1"), "down")
        self.assertEqual(direction(layout, "w1:p2"), "right")
        self.assertEqual(direction(layout, "w1:p3"), "right")
        with self.assertRaises(ValueError):
            direction(layout, "w1:p99")

    def invoke(self, args, env, responses):
        with patch.dict(os.environ, env, clear=True), patch("sys.argv", [str(SCRIPT), *args]), \
                patch("subprocess.check_output", side_effect=responses) as read, \
                patch("subprocess.run") as run:
            MODULE["main"]()
        return read.call_args_list, run.call_args

    def test_tmux_command(self):
        for args, env in [
            (["--backend", "tmux", "%1"], {"HERDR_ENV": "1"}),
            ([], {"TMUX": "/socket", "TMUX_PANE": "%1"}),
        ]:
            reads, call = self.invoke(args, env, ["abcd,160x50,0,0,1"])
            self.assertEqual(reads[0].args[0], ["tmux", "display-message", "-p", "-t", "%1", "#{window_layout}"])
            self.assertEqual(call.args[0], ["tmux", "split-window", "-t", "%1", "-h", "-l", "50%", "-c", "#{pane_current_path}"])
            self.assertEqual(call.kwargs, {"check": True})

    def test_herdr_command(self):
        responses = [
            json.dumps({"result": {"pane": {"pane_id": "w1:p2", "foreground_cwd": "/work/my repo", "cwd": "/old"}}}),
            json.dumps({"result": {"layout": {
                "panes": [{"pane_id": "w1:p2", "rect": rect(80, 0, 80, 50)}],
                "splits": [{"direction": "right", "rect": rect(0, 0, 160, 50)}],
            }}}),
        ]
        for args, env, target in [
            ([], {"HERDR_ENV": "1"}, ["--current"]),
            (["--backend", "herdr", "w1:p2"], {"TMUX": "/socket"}, ["--pane", "w1:p2"]),
        ]:
            reads, call = self.invoke(args, {**env, "HERDR_BIN_PATH": "/custom/herdr"}, responses)
            self.assertEqual(reads[0].args[0], ["/custom/herdr", "pane", "current", *target])
            self.assertEqual(reads[1].args[0], ["/custom/herdr", "pane", "layout", "--pane", "w1:p2"])
            self.assertEqual(call.args[0], ["/custom/herdr", "pane", "split", "--pane", "w1:p2", "--direction", "down", "--ratio", "0.5", "--focus", "--cwd", "/work/my repo"])
            self.assertEqual(call.kwargs, {"check": True})

    def test_no_backend(self):
        with self.assertRaises(SystemExit) as error, patch("sys.stderr"):
            self.invoke([], {}, [])
        self.assertEqual(error.exception.code, 2)


if __name__ == "__main__":
    unittest.main()
