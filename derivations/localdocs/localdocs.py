#!/usr/bin/env python

import argparse

from rich.console import Console
from rich.markdown import Markdown
import requests

parser = argparse.ArgumentParser()
parser.add_argument("resource")
parser.add_argument("--provider-version", required=False, default="v5.82.2")
args = parser.parse_args()

resource = args.resource.replace("aws_", "", 1)

url = f"https://raw.githubusercontent.com/hashicorp/terraform-provider-aws/{args.provider_version}/website/docs/r/{resource}.html.markdown"
r = requests.get(url)
r.raise_for_status()

console = Console()
md = Markdown(r.text)
with console.pager(styles=True, links=True):
    console.print(md)
