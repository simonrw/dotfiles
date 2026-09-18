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
    def run_action(
        self,
        action,
        state,
        focused_workspace="w2",
        focused_tab="w2:t2",
        events=None,
        session=None,
        plugin_environment=True,
        tabs=None,
    ):
        if tabs is None:
            tabs = ["w2:t1", "w2:t2"]
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            calls = root / "calls"
            fake_herdr = root / "herdr"
            fake_herdr.write_text(
                f"""#!/usr/bin/env python3
import json, sys
from pathlib import Path
Path({str(calls)!r}).open("a").write(" ".join(sys.argv[1:]) + "\\n")
if sys.argv[1:3] == ["status", "server"]:
    print(json.dumps({{"session": {session!r}}}))
elif sys.argv[1:3] == ["workspace", "list"]:
    print(json.dumps({{"result": {{"workspaces": [
        {{"workspace_id": "w1", "label": "renamed workspace", "focused": {focused_workspace == "w1"!s}, "active_tab_id": "w1:t1"}},
        {{"workspace_id": "w2", "label": "same label", "focused": {focused_workspace == "w2"!s}, "active_tab_id": {focused_tab!r}}},
        {{"workspace_id": "w3", "label": "same label", "focused": {focused_workspace == "w3"!s}}},
        {{"workspace_id": "w4", "label": "unvisited", "focused": False}}
    ]}}}}))
elif sys.argv[1:3] == ["tab", "list"]:
    print(json.dumps({{"result": {{"tabs": [{{"tab_id": tab}} for tab in {tabs!r}]}}}}))
elif sys.argv[1:3] in (["workspace", "focus"], ["tab", "focus"]):
    print(json.dumps({{"result": {{}}}}))
else:
    raise SystemExit(2)
"""
            )
            fake_herdr.chmod(fake_herdr.stat().st_mode | stat.S_IXUSR)
            state_dir = root / "state/herdr/plugins/dotfiles.last-focus"
            directory = state_dir / "sessions" / session if session else state_dir
            directory.mkdir(parents=True)
            state_path = directory / "focus-history.json"
            if state is not None:
                state_path.write_text(
                    state if isinstance(state, str) else json.dumps(state)
                )
            if session:
                (state_dir / "focus-history.json").write_text(
                    "default session untouched"
                )
            env = os.environ | {
                "HERDR_BIN_PATH": str(fake_herdr),
                "XDG_STATE_HOME": str(root / "state"),
            }
            env.pop("HERDR_PLUGIN_STATE_DIR", None)
            if plugin_environment:
                env["HERDR_PLUGIN_STATE_DIR"] = str(state_dir)
            for workspace_id in events or []:
                subprocess.run(
                    [SCRIPT, "event"],
                    check=True,
                    env=env
                    | {
                        "HERDR_PLUGIN_EVENT_JSON": json.dumps(
                            {
                                "event": "workspace.focused",
                                "data": {"workspace_id": workspace_id},
                            }
                        )
                    },
                )
            completed = subprocess.run(
                [SCRIPT, action], env=env, check=True, capture_output=True, text=True
            )
            self.output = completed.stdout
            result = json.loads(state_path.read_text())
            if session:
                self.assertEqual(
                    (state_dir / "focus-history.json").read_text(),
                    "default session untouched",
                )
            return result, calls.read_text().splitlines()

    def test_last_workspace_uses_old_current_when_event_is_delayed(self):
        state, calls = self.run_action(
            "last-workspace", {"workspace": {"current": "w1"}, "tabs": {}}
        )
        self.assertIn("workspace focus w1", calls)
        self.assertEqual(state["workspace"], {"current": "w1", "previous": "w2"})
        self.assertEqual(state["recent_workspaces"], ["w1", "w2"])

    def test_last_tab_uses_old_current_when_event_is_delayed(self):
        state, calls = self.run_action(
            "last-tab",
            {
                "workspace": {"current": "w2"},
                "tabs": {"w2": {"current": "w2:t1"}},
            },
        )
        self.assertIn("tab focus w2:t1", calls)
        self.assertEqual(state["tabs"]["w2"], {"current": "w2:t1", "previous": "w2:t2"})

    def test_relative_navigation_remembers_each_tab_without_focus_events(self):
        tabs = ["w2:t4", "w2:t2", "w2:t9"]
        for action, route in [
            ("next-tab", ["w2:t4", "w2:t2", "w2:t9", "w2:t4"]),
            ("previous-tab", ["w2:t4", "w2:t9", "w2:t2", "w2:t4"]),
        ]:
            with self.subTest(action=action):
                state = {"workspace": {"current": "w2"}, "tabs": {}}
                for current, target in zip(route, route[1:]):
                    state, calls = self.run_action(
                        action, state, focused_tab=current, tabs=tabs
                    )
                    self.assertIn(f"tab focus {target}", calls)
                    self.assertEqual(
                        state["tabs"]["w2"], {"current": target, "previous": current}
                    )
                state, calls = self.run_action(
                    "last-tab", state, focused_tab=route[-1], tabs=tabs
                )
                self.assertIn(f"tab focus {route[-2]}", calls)

    def test_relative_navigation_with_no_destination_keeps_history(self):
        for action in ["next-tab", "previous-tab"]:
            for tabs in [[], ["w2:t2"]]:
                with self.subTest(action=action, tabs=tabs):
                    history = {"current": "w2:t2", "previous": "w2:t1"}
                    state, calls = self.run_action(
                        action,
                        {"workspace": {"current": "w2"}, "tabs": {"w2": history}},
                        tabs=tabs,
                    )
                    self.assertFalse(any(call.startswith("tab focus") for call in calls))
                    self.assertEqual(state["tabs"]["w2"], history)

    def listed_ids(self):
        return [
            workspace["workspace_id"]
            for workspace in json.loads(self.output)["result"]["workspaces"]
        ]

    def test_focus_history_orders_live_workspaces_and_keeps_latest_labels(self):
        state, _ = self.run_action(
            "list-workspaces",
            {"workspace": {}, "tabs": {}},
            events=["w1", "w3", "closed", "w2", "w2"],
        )
        self.assertEqual(self.listed_ids(), ["w2", "w3", "w1", "w4"])
        self.assertEqual(state["recent_workspaces"], ["w2", "w3", "w1"])
        self.assertEqual(state["workspace"]["previous"], "closed")
        workspaces = json.loads(self.output)["result"]["workspaces"]
        self.assertEqual(workspaces[2]["label"], "renamed workspace")

    def test_old_history_is_migrated_and_live_focus_corrects_delayed_event(self):
        state, _ = self.run_action(
            "list-workspaces",
            {"workspace": {"current": "w3", "previous": "w1"}, "tabs": {}},
            plugin_environment=False,
        )
        self.assertEqual(self.listed_ids(), ["w2", "w3", "w1", "w4"])
        self.assertEqual(state["workspace"], {"current": "w2", "previous": "w3"})

    def test_missing_or_corrupt_state_still_lists_every_workspace(self):
        for state in [None, "invalid json"]:
            with self.subTest(state=state):
                self.run_action("list-workspaces", state, plugin_environment=False)
                self.assertEqual(self.listed_ids(), ["w2", "w1", "w3", "w4"])

    def test_named_session_history_is_separate_from_default(self):
        self.run_action("list-workspaces", None, session="work", events=["w3", "w2"])
        self.assertEqual(self.listed_ids(), ["w2", "w3", "w1", "w4"])

    def test_startup_preserves_history_and_initializes_focused_tab(self):
        state, _ = self.run_action(
            "startup",
            {
                "workspace": {"current": "w2"},
                "tabs": {},
                "recent_workspaces": ["w2", "w3", "w1"],
            },
        )
        self.assertEqual(state["recent_workspaces"], ["w2", "w3", "w1"])
        self.assertEqual(state["tabs"]["w2"]["current"], "w2:t2")


if __name__ == "__main__":
    unittest.main()
