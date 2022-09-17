#!/usr/bin/env python3


import sys
from pathlib import Path
import argparse
import subprocess as sp


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-o", "--output", required=True, help="JSON file name to output", type=Path
    )
    parser.add_argument("-H", "--host", required=True)
    parser.add_argument(
        "-i",
        "--interval",
        required=False,
        type=int,
        default=1,
        help="How often to perform a test",
    )
    parser.add_argument(
        "-t",
        "--time",
        required=False,
        default=3600,
        type=int,
        help="How long to run the test for",
    )
    parser.add_argument(
        "-b",
        "--bind",
        required=False,
        help="IP address to bind to, to e.g. specify a network interface",
    )
    parser.add_argument(
        "-f",
        "--force",
        action="store_true",
        default=False,
        help="If an existing output file exists, then allow overwriting",
    )
    args = parser.parse_args()

    output = args.output.resolve()
    if output.is_file():
        if not args.force:
            print(
                f"Output file {str(output)} exists and `-f/--force` not given. "
                "We cannot continue without overwriting the content that is already there.",
                file=sys.stderr,
            )
            sys.exit(1)

        # Remove the original file
        output.unlink()

    cmd = [
        "iperf3",
        "--time",
        str(args.time),
        "--json",
        "--logfile",
        str(output),
        "--interval",
        str(args.interval),
        "-c",
        args.host,
    ]
    if args.bind is not None:
        cmd.extend(["--bind", args.bind])

    sp.run(cmd)
