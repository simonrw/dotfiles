#!/usr/bin/env python3
"""Export and apply allowlisted Rex preferences through macOS defaults."""

import argparse
import json
import math
import plistlib
import subprocess
import sys
import tempfile
from pathlib import Path

MANIFEST = Path(__file__).with_name("preferences.json")
DOMAIN = "com.superlogical.rex"


def write_arguments(manifest):
    """Validate a manifest and return typed defaults arguments for every key."""
    if not isinstance(manifest, dict) or set(manifest) != {"preferences", "json_data"}:
        raise ValueError("Manifest must contain preferences and json_data objects")
    preferences = manifest["preferences"]
    json_data = manifest["json_data"]
    if not isinstance(preferences, dict) or not isinstance(json_data, dict):
        raise TypeError("preferences and json_data must be objects")
    if preferences.keys() & json_data.keys():
        raise ValueError("A key cannot appear in both preferences and json_data")

    arguments = []
    for key, value in preferences.items():
        supported = (
            isinstance(value, (bool, int, str))
            or isinstance(value, float)
            and math.isfinite(value)
            or isinstance(value, list)
            and all(isinstance(item, str) for item in value)
        )
        if not supported:
            raise ValueError(f"Unsupported preference value for {key}")
        # XML preserves double precision; defaults -float narrows values to float32.
        arguments.append([key, plistlib.dumps(value).decode("utf-8")])
    for key, value in json_data.items():
        data = json.dumps(value, ensure_ascii=False, allow_nan=False).encode("utf-8")
        arguments.append([key, plistlib.dumps(data).decode("utf-8")])
    return arguments


def export_preferences(domain, path, manifest):
    """Refresh only the keys already tracked by the manifest."""
    result = subprocess.run(
        ["defaults", "export", domain, "-"], check=True, capture_output=True
    )
    current = plistlib.loads(result.stdout)
    keys = set(manifest["preferences"]) | set(manifest["json_data"])
    missing = keys - current.keys()
    if missing:
        raise ValueError(
            "Tracked keys missing from defaults: " + ", ".join(sorted(missing))
        )

    updated = {
        "preferences": {key: current[key] for key in manifest["preferences"]},
        "json_data": {},
    }
    for key in manifest["json_data"]:
        if not isinstance(current[key], bytes):
            raise TypeError(f"Expected plist Data for {key}")
        updated["json_data"][key] = json.loads(current[key])
    write_arguments(updated)
    content = json.dumps(
        updated, indent=2, sort_keys=True, ensure_ascii=False, allow_nan=False
    )
    # Replace only after every tracked value has been read and validated.
    with tempfile.NamedTemporaryFile(
        mode="w", encoding="utf-8", dir=path.parent, delete=False
    ) as temp:
        temporary_path = Path(temp.name)
        try:
            temp.write(content + "\n")
            temp.close()
            temporary_path.replace(path)
        finally:
            temporary_path.unlink(missing_ok=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["export", "apply"])
    parser.add_argument("--manifest", type=Path, default=MANIFEST)
    parser.add_argument("--domain", default=DOMAIN)
    args = parser.parse_args()
    if sys.platform != "darwin":
        parser.error("Rex preferences require macOS")
    try:
        manifest = json.loads(args.manifest.read_text(encoding="utf-8"))
        commands = write_arguments(manifest)
        if args.command == "export":
            export_preferences(args.domain, args.manifest, manifest)
            print(f"Exported {len(commands)} preferences to {args.manifest}")
        else:
            for command in commands:
                subprocess.run(["defaults", "write", args.domain, *command], check=True)
            print(f"Applied {len(commands)} preferences. Reopen Rex to load them.")
    except (
        OSError,
        ValueError,
        TypeError,
        OverflowError,
        plistlib.InvalidFileException,
        subprocess.CalledProcessError,
    ) as error:
        parser.exit(1, f"{error}\n")


if __name__ == "__main__":
    main()
