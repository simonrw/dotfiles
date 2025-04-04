#!/usr/bin/env python

from argparse import ArgumentParser
from pathlib import Path
import sys

ROOT = Path(__file__).parent

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

    if dest.is_file():
        if args.remove_all:
            dest.unlink()
            continue

        if not args.force:
            print(
                f"Not installing {dest} as file exists. Add `-f/--force` to overwrite",
                file=sys.stderr,
            )
            continue

        dest.unlink()

    dest.symlink_to(source)
