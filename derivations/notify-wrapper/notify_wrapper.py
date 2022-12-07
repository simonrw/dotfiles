#!/usr/bin/env python

import argparse
import os
import subprocess as sp
from typing import List

import requests


class Ntfy:
    def __init__(self) -> None:
        self.session = requests.Session()
        self.topic_name = os.environ["NTFY_TOPIC"]

    def publish(self, message: str, tags: List[str]) -> None:
        url = f"https://ntfy.sh/{self.topic_name}"
        headers = {
            "Title": "Watched task finished",
            "Tags": ",".join(tags),
        }
        r = self.session.post(url, data=message, headers=headers)
        r.raise_for_status()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("cmd")
    parser.add_argument("-m", "--message", required=False)
    args = parser.parse_args()
    ntfy = Ntfy()

    ret = sp.run(args.cmd, shell=True)
    if ret.returncode == 0:
        message = args.message if args.message is not None else f"Task {args.cmd} success"
        tag = "tada"
    else:
        message = args.message if args.message is not None else f"Task {args.cmd} failed"
        tag = "x"

    ntfy.publish(message, tags=[tag])
