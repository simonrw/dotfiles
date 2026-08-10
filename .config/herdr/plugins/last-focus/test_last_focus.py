#!/usr/bin/env python3

import json
import os
import stat
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).with_name("last-focus")


class LastFocusTest(unittest.TestCase):
    def run_action(self, action, state, focused_workspace="w2", focused_tab="w2:t2"):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            calls = root / "calls"
            fake_herdr = root / "herdr"
            fake_herdr.write_text(
                f'''#!/usr/bin/env python3
import json, sys
from pathlib import Path
Path({str(calls)!r}).open("a").write(" ".join(sys.argv[1:]) + "\\n")
if sys.argv[1:3] == ["workspace", "list"]:
    print(json.dumps({{"result": {{"workspaces": [
        {{"workspace_id": "w1", "focused": {str(focused_workspace == "w1")}, "active_tab_id": "w1:t1"}},
        {{"workspace_id": "w2", "focused": {str(focused_workspace == "w2")}, "active_tab_id": {focused_tab!r}}}
    ]}}}}))
elif sys.argv[1:3] == ["tab", "list"]:
    print(json.dumps({{"result": {{"tabs": [
        {{"tab_id": "w2:t1"}}, {{"tab_id": "w2:t2"}}
    ]}}}}))
elif sys.argv[1:3] in (["workspace", "focus"], ["tab", "focus"]):
    print(json.dumps({{"result": {{}}}}))
else:
    raise SystemExit(2)
'''
            )
            fake_herdr.chmod(fake_herdr.stat().st_mode | stat.S_IXUSR)
            (root / "focus-history.json").write_text(json.dumps(state))
            env = os.environ | {
                "HERDR_BIN_PATH": str(fake_herdr),
                "HERDR_PLUGIN_STATE_DIR": str(root),
            }
            subprocess.run([SCRIPT, action], env=env, check=True)
            result = json.loads((root / "focus-history.json").read_text())
            return result, calls.read_text().splitlines()

    def test_last_workspace_uses_old_current_when_event_is_delayed(self):
        state, calls = self.run_action(
            "last-workspace", {"workspace": {"current": "w1"}, "tabs": {}}
        )
        self.assertIn("workspace focus w1", calls)
        self.assertEqual(state["workspace"], {"current": "w1", "previous": "w2"})

    def test_last_tab_uses_old_current_when_event_is_delayed(self):
        state, calls = self.run_action(
            "last-tab",
            {
                "workspace": {"current": "w2"},
                "tabs": {"w2": {"current": "w2:t1"}},
            },
        )
        self.assertIn("tab focus w2:t1", calls)
        self.assertEqual(
            state["tabs"]["w2"], {"current": "w2:t1", "previous": "w2:t2"}
        )


if __name__ == "__main__":
    unittest.main()
