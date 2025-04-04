#!/usr/bin/env python

import argparse
import os
import subprocess as sp
import shlex
from typing import List

import requests


class Ntfy:
    def __init__(self) -> None:
        self.session = requests.Session()
        self.topic_name = os.environ["NTFY_TOPIC"]
        self.auth_token = os.getenv("NTFY_TOKEN")

    def publish(self, message: str, tags: List[str]) -> None:
        url = f"https://ntfy.sh/{self.topic_name}"
        headers = {
            "Title": "Watched task finished",
            "Tags": ",".join(tags),
        }
        if self.authorized:
            headers["Authorization"] = f"Bearer {self.auth_token}"

        r = self.session.post(url, data=message.encode("utf-8"), headers=headers)
        r.raise_for_status()

    @property
    def authorized(self) -> bool:
        return self.auth_token is not None


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-m", "--message", required=False)
    args, cmd = parser.parse_known_args()
    ntfy = Ntfy()

    command = shlex.join(cmd)
    ret = sp.run(command, shell=True)
    if ret.returncode == 0:
        message = args.message if args.message is not None else f"Task {command} success"
        tag = "tada"
    else:
        message = args.message if args.message is not None else f"Task {command} failed"
        tag = "x"

    ntfy.publish(message, tags=[tag])
