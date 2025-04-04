#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p python3 yt-dlp


import subprocess as sp
import argparse
from pathlib import Path
import logging
from typing import Optional
from enum import Enum, auto


class VideoType(Enum):
    Video = auto()
    Playlist = auto()
    Channel = auto()


class Downloader:
    def download(self, url: str, output: Optional[Path]) -> None:
        logging.debug(f"{url} {output}")
        output_dir = output if output is not None else Path.cwd()
        logging.info(f"Outputting to {output_dir}")

        output_format = output_dir.joinpath(
            "%(upload_date)s-%(playlist_index)d-%(title)s.%(ext)s"
        )

        cmd = [
            "yt-dlp",
            "--format",
            "bestvideo+bestaudio",
            url,
            "--output",
            str(output_format),
        ]
        sp.run(cmd, check=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "url", help="URL of item to download (channel, video, playlist)"
    )
    parser.add_argument(
        "-o",
        "--output-dir",
        required=False,
        type=Path,
        help="Directory to download the file to (defaults to cwd)",
    )
    args = parser.parse_args()

    downloader = Downloader()
    downloader.download(args.url, args.output_dir)
