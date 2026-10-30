#!/usr/bin/env python3
"""Exercise preference sync against disposable macOS defaults domains."""

import json
import plistlib
import subprocess
import sys
import tempfile
import unittest
import uuid
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / ".config/rex/preferences.py"


@unittest.skipUnless(sys.platform == "darwin", "requires macOS defaults")
class RexPreferences(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.path = Path(self.temp.name) / "preferences.json"
        self.domain = "com.example.rex-preferences-test." + uuid.uuid4().hex
        self.addCleanup(
            subprocess.run, ["defaults", "delete", self.domain], capture_output=True
        )

    def seed(self, values):
        path = Path(self.temp.name) / "seed.plist"
        path.write_bytes(plistlib.dumps(values))
        subprocess.run(["defaults", "import", self.domain, str(path)], check=True)

    def current(self):
        return plistlib.loads(subprocess.check_output(["defaults", "export", self.domain, "-"]))

    def run_sync(self, command):
        return subprocess.run(
            [sys.executable, str(SCRIPT), command, "--manifest", str(self.path),
             "--domain", self.domain], capture_output=True, text=True, timeout=15, check=False,
        )

    def test_roundtrip_preserves_types_and_untracked_preferences(self):
        preferences = {
            "font": "JetBrains Mono Bold", "smooth": True, "copy": False,
            "size": 14, "opacity": 0.42167968750000007,
            "hidden": ["commandPalette", "newTab"], "empty": [],
        }
        themes = [{"id": "custom-test", "name": "Thème", "colors": [0.5, 1]}]
        manifest = {"preferences": preferences, "json_data": {"themes": themes}}
        self.path.write_text(json.dumps(manifest), encoding="utf-8")
        self.seed({**preferences, "themes": json.dumps(themes).encode(), "receipts": "local"})
        result = self.run_sync("export")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(self.path.read_text()), manifest)

        self.seed({"size": 99, "receipts": "other-host", "lastSession": "keep"})
        for _ in range(2):
            result = self.run_sync("apply")
            self.assertEqual(result.returncode, 0, result.stderr)
            actual = self.current()
            theme_data = actual.pop("themes")
            self.assertIsInstance(theme_data, bytes)
            self.assertEqual(json.loads(theme_data), themes)
            expected = {**preferences, "receipts": "other-host", "lastSession": "keep"}
            self.assertEqual(actual, expected)
            self.assertEqual({k: type(v) for k, v in actual.items()},
                             {k: type(v) for k, v in expected.items()})

    def test_missing_export_key_leaves_manifest_unchanged(self):
        self.seed({"receipts": "untracked"})
        for section in ["preferences", "json_data"]:
            with self.subTest(section=section):
                manifest = {"preferences": {}, "json_data": {}}
                manifest[section]["missing"] = "value"
                original = json.dumps(manifest).encode()
                self.path.write_bytes(original)
                result = self.run_sync("export")
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("missing", result.stderr)
                self.assertEqual(self.path.read_bytes(), original)

    def test_invalid_apply_leaves_domain_unchanged(self):
        original = {"size": 14, "receipts": "keep"}
        self.seed(original)
        self.path.write_text(json.dumps({
            "preferences": {"size": 99, "invalid": {"nested": True}}, "json_data": {},
        }))
        result = self.run_sync("apply")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.current(), original)


if __name__ == "__main__":
    unittest.main()
