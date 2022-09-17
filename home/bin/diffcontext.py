#!/usr/bin/env python

import re
import sys


DIFF_RE = re.compile(r"^.*diff\s--git\s+a/([^\s]+)")
CHUNK_RE = re.compile(r"^.*@@\s+")
# https://stackoverflow.com/a/14693789/56711
ANSI_ESCAPE = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
YELLOW = "\x1b[33m"
RESET = "\x1b[0m"


def main():
    context = None

    for line in sys.stdin:
        line = line.strip()
        match = DIFF_RE.match(line)
        if match:
            context = match.group(1)
            continue

        match = CHUNK_RE.match(line)
        if match:
            assert context is not None
            print(f"{line} {YELLOW}({context}){RESET}")
            continue

        print(line)


if __name__ == "__main__":
    main()
