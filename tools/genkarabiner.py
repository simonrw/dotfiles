#!/usr/bin/env python


import json
from pathlib import Path


def gen_rule(key, mod):
    assert mod in {"left_shift", "right_shift"}

    return {
        "type": "basic",
        "from": {"key_code": key, "modifiers": {"mandatory": [mod]}},
    }


manipulators = []
for key in ["q", "w", "e", "r", "t", "a", "s", "d", "f", "g", "z", "x", "c", "v"]:
    rule = gen_rule(key, "left_shift")
    manipulators.append(rule)

for key in [
    "y",
    "u",
    "i",
    "o",
    "p",
    "h",
    "j",
    "k",
    "l",
    "semicolon",
    "b",
    "n",
    "m",
    "comma",
    "period",
    "slash",
]:
    rule = gen_rule(key, "right_shift")
    manipulators.append(rule)

body = {
    "title": "Learn to touch type",
    "rules": [
        {"description": "Disable incorrect shift key", "manipulators": manipulators}
    ],
}

out_dir = Path.home() / ".config" / "karabiner" / "assets" / "complex_modifications"
assert out_dir.is_dir()
out_file = out_dir / "touchtype.json"

with out_file.open("w") as out:
    json.dump(body, out, indent=2)
