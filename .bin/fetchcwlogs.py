#!/usr/bin/env python

"""
Bulk fetch logs from cloudwatch
"""


import boto3
import json
import shutil
from pathlib import Path
import argparse


description = """Bulk fetches logs from cloudwatch

Given a log group and log stream, download all of the logs into a directory.
"""

parser = argparse.ArgumentParser(description=description)
parser.add_argument("-g", "--group", required=True,
                    help="Log group to download")
parser.add_argument("-s", "--stream", required=True, help="Log stream")
parser.add_argument("-o", "--output", required=True,
                    type=Path, help="Output directory")
parser.add_argument("--start-time", required=False, type=int,
                    help="Start time (millisecond timestamp UTC, optional)")
parser.add_argument("--end-time", required=False, type=int,
                    help="End time (millisecond timestamp UTC, optional)")
args = parser.parse_args()

log_group_name = args.group
log_stream_name = args.stream

args.output.mkdir(parents=True, exist_ok=True)

logs = boto3.client("logs")

log_idx = 0
next_token = None
while True:
    params = dict(
        logGroupName=log_group_name,
        logStreamName=log_stream_name,
        startFromHead=True,
    )

    if args.start_time is not None:
        params["startTime"] = args.start_time
    if args.end_time is not None:
        params["endTime"] = args.end_time
    if next_token is not None:
        params["nextToken"] = next_token

    current_logs = logs.get_log_events(**params)

    if "events" not in current_logs or len(current_logs["events"]) == 0:
        break

    if "nextForwardToken" in current_logs:
        next_token = current_logs["nextForwardToken"]
    else:
        break

    path = args.output.joinpath(f"logs{log_idx}.json")
    with path.open("w") as outfile:
        json.dump(current_logs, outfile)

    log_idx += 1

    print(f"\rDownloaded {log_idx:3d} logs... ", end="")

print("Done")
