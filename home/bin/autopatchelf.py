#!/usr/bin/env python

import argparse
import logging
import sys
import os
import subprocess as sp
from pathlib import Path
from typing import Iterator
import re

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger()


SO_RE = re.compile(r".*\.so(\..*)?")


def walk(root: Path) -> Iterator[Path]:
    for p in root.iterdir():
        if p.is_dir():
            yield from walk(p)
            continue
        yield p.resolve()


def is_elf(path: Path) -> bool:
    with open(path, "rb") as infile:
        magic = infile.read(4)

    return magic == b"\x7fELF"


def should_update(path: Path) -> bool:
    if not is_elf(path):
        return False

    if SO_RE.match(str(path)):
        return False

    return True


def file_is_patched(path: Path) -> bool:
    cmd = ["patchelf", "--print-interpreter", path]
    logger.debug(f"Running command {cmd}")
    try:
        res = sp.check_output(cmd, stderr=sp.PIPE)
        linker = Path(res.decode().strip())
        logger.debug(f"found interpreter {linker}")
        return linker.is_file()
    except sp.CalledProcessError as e:
        if "statically linked" in e.stderr.decode():
            return True

        raise


def patch_file(path: Path, dynamic_linker_path: str):
    if file_is_patched(path):
        logger.info("file patched, skipping")
        return

    cmd = ["patchelf", "--set-interpreter", dynamic_linker_path, str(path)]
    logger.debug(f"Running command {cmd}")
    sp.check_call(cmd)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("path", type=Path)
    parser.add_argument("-v", "--verbose", action="count", default=0)
    args = parser.parse_args()

    if args.verbose == 1:
        logger.setLevel(logging.INFO)
    elif args.verbose > 1:
        logger.setLevel(logging.DEBUG)

    try:
        nix_cc_path = os.environ["NIX_CC"]
        logger.debug(f"Nix CC path: {nix_cc_path}")
    except KeyError:
        print("Not in nix environment", file=sys.stderr)
        raise SystemExit(1)

    dynamic_linker_path_file = Path(nix_cc_path) / "nix-support" / "dynamic-linker"
    logger.debug(f"dynamic linker path file: {dynamic_linker_path_file}")
    with dynamic_linker_path_file.open() as infile:
        dynamic_linker_path = infile.read().strip()

    logger.debug(f"dynamic linker path: {dynamic_linker_path}")

    for child in walk(args.path):
        if not should_update(child):
            continue

        logger.info(f"Found update candidate: {child}")

        patch_file(child, dynamic_linker_path)
