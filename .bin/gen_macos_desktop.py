#!/usr/bin/env python3

from tempfile import NamedTemporaryFile
from argparse import ArgumentParser
import json
import struct
import subprocess as sp
import zlib


wallpapper_config = [
    {
        "fileName": "light.png",
        "isPrimary": True,
        "isForLight": True,
    },
    {
        "fileName": "dark.png",
        "isForDark": True,
    },
]


def save_png(filename, r, g, b):
    def chunk(tag, data):
        return (
            struct.pack(">I", len(data))
            + tag
            + data
            + struct.pack(">I", zlib.crc32(tag + data) & 0xFFFFFFFF)
        )

    # 1x1 pixel PNG data
    pixel = bytes([r, g, b])
    width, height = 1, 1
    ihdr = struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0)
    idat = zlib.compress(b"\x00" + pixel)

    with open(filename, "wb") as f:
        f.write(b"\x89PNG\r\n\x1a\n")
        f.write(chunk(b"IHDR", ihdr))
        f.write(chunk(b"IDAT", idat))
        f.write(chunk(b"IEND", b""))


# import pdb; pdb.set_trace()

parser = ArgumentParser()
parser.add_argument("-o", "--output", required=True)
args = parser.parse_args()

with NamedTemporaryFile("wb", suffix=".png") as light_file:
    save_png(light_file.name, 209, 228, 253)  # Light Grey
    light_file.flush()

    wallpapper_config[0]["fileName"] = light_file.name

    with NamedTemporaryFile("wb", suffix=".png") as dark_file:
        save_png(dark_file.name, 89, 91, 111)  # Dark Navy
        dark_file.flush()

        wallpapper_config[1]["fileName"] = dark_file.name

        with NamedTemporaryFile("w", suffix=".json") as config_file:
            json.dump(wallpapper_config, config_file)
            config_file.flush()

            sp.check_call(
                ["wallpapper", "-i", config_file.name, "-o", args.output]
            )
