#!/usr/bin/env python


import argparse
import pathlib
import logging


logging.basicConfig(
    level=logging.WARNING, format="[%(asctime)s] %(levelname)7s:%(message)s"
)
logger = logging.getLogger("deploy")


def deploy():
    parser = argparse.ArgumentParser()
    parser.add_argument("-n", "--dry-run", action="store_true", default=False)
    parser.add_argument("-v", "--verbose", action="count")
    args = parser.parse_args()

    if args.verbose is not None:
        if args.verbose == 1:
            logger.setLevel(logging.INFO)
        elif args.verbose > 1:
            logger.setLevel(logging.DEBUG)

    logger.info("deploying dotfiles")
    logger.debug("debug logging enabled")


if __name__ == "__main__":
    deploy()
