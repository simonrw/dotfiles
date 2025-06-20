#!/opt/homebrew/opt/python/bin/python3

import enum
import subprocess as sp
import logging

logging.basicConfig(level=logging.DEBUG, filename="/tmp/themechanges.log", filemode="a", format="[%(asctime)s] %(message)s")
LOG = logging.getLogger(__name__)


class Theme(enum.Enum):
    Dark = enum.auto()
    Light = enum.auto()


def is_dark_theme() -> Theme:
    cmd = ["defaults", "read", "-g", "AppleInterfaceStyle"]
    result = sp.run(cmd)
    if result.returncode == 0:
        return Theme.Dark
    else:
        return Theme.Light

def update_k9s_theme(theme: Theme):
    LOG.info("Updating k9s to theme: %s", theme)

LOG.info("Starting")
theme = is_dark_theme()
update_k9s_theme(theme)
