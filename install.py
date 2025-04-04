#!/usr/bin/env python

from argparse import ArgumentParser
from pathlib import Path
import sys

ROOT = Path(__file__).parent


def link_to(source: Path, dest: Path, remove_all: bool, force: bool):
    if dest.exists():
        if args.remove_all:
            dest.unlink()
            return

        if not args.force:
            print(
                f"Not installing {dest} as file exists. Add `-f/--force` to overwrite",
                file=sys.stderr,
            )
            return

        dest.unlink()

    dest.symlink_to(source)


parser = ArgumentParser()
_ = parser.add_argument(
    "-r",
    "--remove-all",
    action="store_true",
    default=False,
    help="Remove all tracked files",
)
_ = parser.add_argument(
    "-f", "--force", action="store_true", default=False, help="Overwrite existing files"
)
args = parser.parse_args()


# Install top level files
top_level_files = [".ideavimrc"]
for file in top_level_files:
    source = ROOT / file
    assert source.is_file()
    dest = Path.home() / file
    link_to(source, dest, args.remove_all, args.force)

config_root = ROOT / ".config"
config_files = config_root.walk()
for root, children, _ in config_files:
    if root != config_root:
        continue

    for child in children:
        source = root / child
        assert source.exists()
        dest = Path.home() / ".config" / child
        link_to(source, dest, args.remove_all, args.force)
