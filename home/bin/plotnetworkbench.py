#!/usr/bin/env python3

import matplotlib.pyplot as plt
import numpy as np
import logging
import json
import argparse
from typing import NamedTuple, List


plt.style.use("ggplot")

logging.basicConfig(level=logging.WARNING)
log = logging.getLogger(__name__)
log.setLevel(logging.DEBUG)


class Dataset(NamedTuple):
    time: List[float]
    speed: List[float]
    dropouts: List[int]
    label: str


def convert_rate(bits_per_second: float) -> float:
    return bits_per_second / 1024 / 1024


def load_results_from(fobj):
    log.info("loading results from %s", fobj.name)
    time, speed, dropouts = [], [], []

    data = json.load(fobj)

    for reading in data["intervals"]:
        time.append(reading["sum"]["start"])
        speed.append(convert_rate(reading["sum"]["bits_per_second"]))
        dropouts.append(reading["sum"]["retransmits"])

    # Subtract the zero time in case the start point is not zero
    time = [item - time[0] for item in time]

    return Dataset(time, speed, dropouts, label=fobj.name)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--file", nargs="+", type=argparse.FileType("r"))
    args = parser.parse_args()

    fig, axes = plt.subplots(2, 1, sharex=True, figsize=(11, 8))
    speed_ax, dropouts_ax = axes

    datasets = [load_results_from(f) for f in args.file]

    drawstyle = "steps-post"
    for d in datasets:
        speed_ax.plot(d.time, d.speed, label=d.label, drawstyle=drawstyle)
        dropouts_ax.plot(d.time, d.dropouts, label=d.label, drawstyle=drawstyle)

    speed_ax.set(ylabel="Speed [Mbps]")
    dropouts_ax.set(xlabel="Time [s]", ylabel="Retransmits")

    for ax in axes:
        ax.legend(loc="best")

    fig.tight_layout()
    plt.show()
