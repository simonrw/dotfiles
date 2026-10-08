#!/usr/bin/env python3
"""Live picker regression. Requires a running Rex server and fzf on PATH."""

import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import time
import uuid

ROOT = Path(__file__).resolve().parents[1]
REX = ROOT / ".bin/rex"


def rex(*args):
    return subprocess.check_output(
        [str(REX), "--autostart=false", *args], text=True, timeout=10,
    )


with tempfile.TemporaryDirectory(prefix="rex-fzf-test-", dir="/tmp") as directory:
    label = "rex-fzf-test-" + uuid.uuid4().hex[:8]
    created = json.loads(rex(
        "new", label, "--cwd", directory, "--json", "--shell", "none", "--keep-open", "--",
        "/usr/bin/env", f"PATH={ROOT / '.bin'}:{os.environ['PATH']}", "SESSION_BACKEND=rex",
        "FZF_DEFAULT_OPTS=--tiebreak begin --ansi --no-mouse --tabstop 4 --inline-info --color dark",
        "/bin/bash", str(ROOT / ".bin/tmux-session-history"),
    ))
    session_id = created["session_id"]
    block_id = created["initial_windows"][0]["block_ids"][0]
    try:
        deadline = time.monotonic() + 5
        screen = ""
        while time.monotonic() < deadline:
            screen = rex("capture", "--block", block_id, "--trim")
            # An empty query and visible session row prove that terminal replies
            # neither starved the input pipeline nor became fzf search text.
            if label in screen and re.search(r">\s+< [1-9]\d*/[1-9]\d*", screen):
                break
            time.sleep(0.1)
        else:
            raise AssertionError(f"Picker did not show sessions with an empty query:\n{screen}")
        print("Rex's real fzf picker displays sessions with an empty query.")
    finally:
        rex("kill", session_id)
