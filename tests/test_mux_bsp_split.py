"""Run with: python3 .bin/test_mux_bsp_split.py"""

import json
import os
import runpy
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

SCRIPT = Path.home() / ".bin" / "mux-bsp-split"
assert SCRIPT.is_file()

MODULE = runpy.run_path(str(SCRIPT))


def rect(x, y, width, height):
    return dict(x=x, y=y, width=width, height=height)


class BspSplitTest(unittest.TestCase):
    def test_herdr_directions(self):
        direction = MODULE["herdr_direction"]
        layout = {
            "panes": [{"pane_id": "w1:p1", "rect": rect(0, 0, 160, 50)}],
            "splits": [],
        }
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

    def test_dimensions_override_parent_direction(self):
        for width, height, herdr in [
            (160, 50, "right"),
            (80, 50, "down"),
            (40, 80, "down"),
            (100, 50, "down"),
            (101, 50, "right"),
        ]:
            with self.subTest(width=width, height=height):
                for parent in ["right", "down"]:
                    layout = {
                        "panes": [
                            {"pane_id": "w1:p1", "rect": rect(0, 0, width, height)}
                        ],
                        "splits": [{"direction": parent, "rect": rect(0, 0, 200, 100)}],
                    }
                    self.assertEqual(MODULE["herdr_direction"](layout, "w1:p1"), herdr)

    def invoke(self, args, env, responses):
        with (
            patch.dict(os.environ, env, clear=True),
            patch("sys.argv", [str(SCRIPT), *args]),
            patch("subprocess.check_output", side_effect=responses) as read,
            patch("subprocess.run") as run,
        ):
            MODULE["main"]()
        return read.call_args_list, run.call_args

    def test_herdr_command(self):
        responses = [
            json.dumps(
                {
                    "result": {
                        "pane": {
                            "pane_id": "w1:p2",
                            "foreground_cwd": "/work/my repo",
                            "cwd": "/old",
                        }
                    }
                }
            ),
            json.dumps(
                {
                    "result": {
                        "layout": {
                            "panes": [
                                {"pane_id": "w1:p2", "rect": rect(80, 0, 80, 50)}
                            ],
                            "splits": [
                                {"direction": "right", "rect": rect(0, 0, 160, 50)}
                            ],
                        }
                    }
                }
            ),
        ]
        for args, env, target in [
            ([], {"HERDR_ENV": "1"}, ["--current"]),
            (["--backend", "herdr", "w1:p2"], {"TMUX": "/socket"}, ["--pane", "w1:p2"]),
        ]:
            reads, call = self.invoke(
                args, {**env, "HERDR_BIN_PATH": "/custom/herdr"}, responses
            )
            self.assertEqual(
                reads[0].args[0], ["/custom/herdr", "pane", "current", *target]
            )
            self.assertEqual(
                reads[1].args[0], ["/custom/herdr", "pane", "layout", "--pane", "w1:p2"]
            )
            self.assertEqual(
                call.args[0],
                [
                    "/custom/herdr",
                    "pane",
                    "split",
                    "--pane",
                    "w1:p2",
                    "--direction",
                    "down",
                    "--ratio",
                    "0.5",
                    "--focus",
                    "--cwd",
                    "/work/my repo",
                ],
            )
            self.assertEqual(call.kwargs, {"check": True})

    def test_no_backend(self):
        with self.assertRaises(SystemExit) as error, patch("sys.stderr"):
            self.invoke([], {}, [])
        self.assertEqual(error.exception.code, 2)


@unittest.skipUnless(shutil.which("tmux"), "tmux is not installed")
class TmuxSplitTest(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory(prefix="mux-")
        self.addCleanup(directory.cleanup)
        self.directory = Path(directory.name).resolve()
        self.socket = str(self.directory / "tmux.sock")
        self.env = dict(os.environ)
        self.env.pop("TMUX", None)
        self.env.pop("TMUX_PANE", None)
        self.addCleanup(
            subprocess.run,
            ["tmux", "-S", self.socket, "kill-server"],
            env=self.env,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        self.pane = self.tmux(
            "-f",
            "/dev/null",
            "new-session",
            "-d",
            "-x",
            "201",
            "-y",
            "50",
            "-c",
            str(self.directory),
            "-P",
            "-F",
            "#{pane_id}",
            "sleep",
            "300",
        )
        self.tmux("set-option", "-g", "status", "off")
        self.tmux("resize-window", "-x", "201", "-y", "50")
        self.env.update(TMUX=f"{self.socket},0,0", TMUX_PANE=self.pane)

    def tmux(self, *args):
        return subprocess.check_output(
            ["tmux", "-S", self.socket, *args],
            env=self.env,
            text=True,
        ).strip()

    def split(self, *args):
        return subprocess.check_output(
            [sys.executable, str(SCRIPT), *args],
            env=self.env,
            text=True,
        ).strip()

    def panes(self):
        return self.tmux(
            "list-panes",
            "-F",
            "#{pane_width}\t#{pane_height}\t#{pane_left}\t#{pane_top}\t#{pane_current_path}",
        ).splitlines()

    def test_print_only_dimensions(self):
        for width, expected in [(100, "-v"), (101, "-h")]:
            with self.subTest(width=width):
                self.tmux("resize-window", "-x", str(width), "-y", "50")
                before = self.panes()
                self.assertEqual(self.split("--print-only"), expected)
                self.assertEqual(self.panes(), before)

    def test_target_selection(self):
        other = self.tmux(
            "split-window",
            "-h",
            "-l",
            "40",
            "-c",
            str(self.directory),
            "-P",
            "-F",
            "#{pane_id}",
            "sleep",
            "300",
        )
        self.env["TMUX_PANE"] = other
        self.assertEqual(
            self.split("--backend", "tmux", "--print-only", self.pane), "-h"
        )
        self.assertEqual(self.split("--print-only"), "-v")

    def test_horizontal_split(self):
        self.split()
        cwd = str(self.directory)
        self.assertEqual(
            self.panes(), [f"100\t50\t0\t0\t{cwd}", f"100\t50\t101\t0\t{cwd}"]
        )

    def test_zoomed_vertical_split(self):
        target = self.tmux(
            "split-window",
            "-h",
            "-l",
            "40",
            "-c",
            str(self.directory),
            "-P",
            "-F",
            "#{pane_id}",
            "sleep",
            "300",
        )
        self.tmux("resize-pane", "-Z", "-t", target)
        before = self.panes()
        self.assertEqual(self.split("--print-only", target), "-v")
        self.assertEqual(self.panes(), before)
        self.split("--backend", "tmux", target)
        cwd = str(self.directory)
        self.assertEqual(
            self.panes(),
            [
                f"160\t50\t0\t0\t{cwd}",
                f"40\t24\t161\t0\t{cwd}",
                f"40\t25\t161\t25\t{cwd}",
            ],
        )


if __name__ == "__main__":
    unittest.main()
