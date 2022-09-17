#!/usr/bin/env python3

"""
Downloads the 1080@60 video and highest quality audio and merges with ffmpeg
"""

import argparse
import time
import os
import logging
from typing import Optional, List, Dict, Callable, Any, Tuple, Type
try:
    from typing_extensions import Literal
except ImportError:
    class LiteralCls:
        def __getitem__(self, idx):
            return Any

    Literal = LiteralCls()
from types import TracebackType
import subprocess as sp
from concurrent.futures import ThreadPoolExecutor, as_completed


logging.basicConfig(level=logging.INFO, format="[%(asctime)s|%(thread)d]: %(message)s")

AUDIO_FORMAT = 140
VIDEO_FORMAT = 299


class ExponentialBackoff:
    def __init__(
        self,
        fn: Callable[..., Any],
        args: Optional[Tuple[Any]] = None,
        kwargs: Optional[Dict[str, Any]] = None,
        tries: int = 6,
    ) -> None:
        self.fn = fn
        self.args = args if args is not None else []
        self.kwargs = kwargs if kwargs is not None else {}
        self.tries = tries
        self._count = 0
        self._sleep_time = 1
        self._exc: Optional[Exception] = None
        self._logger = logging.getLogger("backoff")
        self._logger.setLevel(logging.WARNING)

    def run(self) -> Any:
        self._logger.info("running cmd")
        while self._count < self.tries:
            try:
                print(self.args, self.kwargs)
                res = self.fn(*self.args, **self.kwargs)
                self._logger.info("success")
                return res
            except Exception as e:
                self._logger.exception("exception %s, backing off", e)
                self._exc = e
                self._count += 1
                self._logger.warning("sleeping for %d seconds", self._sleep_time)
                time.sleep(self._sleep_time)
                self._sleep_time *= 2

        # if we have reached here then we have timed out
        if self._exc:
            raise self._exc


class YoutubeDownload:
    def __init__(self, url: str, output_format: Optional[str] = None) -> None:
        self.url = url
        # Hidden setting
        self._disable_exponential_backoff = False

        if output_format is None:
            output_format = "%(playlist_index)03d-%(title)s.%(ext)s"
        self.output_format = output_format

    def get_filename(self, file_format: int, playlist_item: int) -> str:
        cmd = [
            "youtube-dl",
            "--format",
            str(file_format),
            "--get-filename",
            "--output",
            self.output_format,
            "--playlist-item",
            str(playlist_item),
            self.url,
        ]

        stdout: bytes
        if self._disable_exponential_backoff:
            stdout = sp.check_output(cmd)
        else:
            backoff = ExponentialBackoff(sp.check_output, args=(cmd,))
            stdout = backoff.run()
        return stdout.decode().strip()

    def download(self, file_format: int, playlist_item: int) -> None:
        cmd = [
            "youtube-dl",
            "--format",
            str(file_format),
            "--playlist-item",
            str(playlist_item),
            "--output",
            self.output_format,
            self.url,
        ]
        logging.debug("cmd %s", cmd)
        backoff = ExponentialBackoff(
            sp.check_call, args=(cmd,), kwargs={"stdout": sp.PIPE}
        )
        backoff.run()

    def num_items(self) -> int:
        cmd = ["youtube-dl", "--flat-playlist", "--dump-json", self.url]
        backoff = ExponentialBackoff(sp.check_output, args=(cmd,))
        stdout = backoff.run()
        info = stdout.decode().strip()
        nentries = len(info.split("\n"))
        return nentries


class Downloader:
    def __init__(self, ytd: YoutubeDownload, item_id: int, format: int) -> None:
        self.ytd = ytd
        self.item_id = item_id
        self.format = format
        self._filename: Optional[str] = None
        logging.debug("created downloader")

    @property
    def filename(self) -> str:
        if self._filename is None:
            logging.debug("filename not cached, fetching from network")
            self._filename = self.ytd.get_filename(self.format, self.item_id)
        return self._filename

    def download(self) -> str:
        logging.info("downloading item %d, format %d", self.item_id, self.format)
        self.ytd.download(self.format, self.item_id)
        self.validate()
        return self.filename

    def validate(self) -> None:
        logging.debug("valdating file %s", self.filename)
        if not os.path.isfile(self.filename):
            raise RuntimeError(f"cannot find downloaded file {self.filename}")


class MergedDownloader:
    def __init__(self, item_id: int, ytd: YoutubeDownload) -> None:
        self.item_id = item_id
        self.ytd = ytd

    def download_and_merge(self) -> None:
        audio_fname = self.download_audio()
        video_fname = self.download_video()

        logging.info(f"Got audio filename {audio_fname}")
        logging.info(f"Got video filename {video_fname}")

        self.merge(audio_fname, video_fname)
        self.clearup(audio_fname)
        self.clearup(video_fname)

    def download_audio(self) -> str:
        dl = Downloader(ytd=self.ytd, item_id=self.item_id, format=AUDIO_FORMAT)
        return dl.download()

    def download_video(self) -> str:
        dl = Downloader(ytd=self.ytd, item_id=self.item_id, format=VIDEO_FORMAT)
        return dl.download()

    def merge(self, audio_fname: str, video_fname: str) -> None:
        logging.info("merging audio and video formats (%s)", video_fname)
        stub, ext = os.path.splitext(video_fname)
        output_fname = f"{stub}-merged{ext}"
        cmd = [
            "ffmpeg",
            "-loglevel",
            "error",
            "-y",
            "-i",
            audio_fname,
            "-i",
            video_fname,
            "-strict",
            "-2",
            "-codec",
            "copy",
            output_fname,
        ]
        logging.debug("cmd %s", cmd)
        sp.check_call(cmd, stdout=sp.PIPE)

    def clearup(self, filename: str) -> None:
        if os.path.isfile(filename):
            logging.info("removing %s", filename)
            os.remove(filename)
        else:
            logging.info("no cleanup required for %s", filename)


class PlaylistDownloader:
    def __init__(
        self,
        url: str,
        n_items: Optional[int] = None,
        nthreads: int = 16,
        output_format: Optional[str] = None,
    ) -> None:
        self.url = url
        self.n_items = n_items
        self.nthreads = nthreads
        self._pool: Optional[ThreadPoolExecutor] = None
        self.ytd = YoutubeDownload(url, output_format)

    def __enter__(self) -> "PlaylistDownloader":
        self._pool = ThreadPoolExecutor(self.nthreads)
        if self.n_items is None:
            self.n_items = self._fetch_n_items()
        return self

    def __exit__(
        self,
        exc_ty: Optional[Type[BaseException]],
        exc_val: Optional[BaseException],
        exc_tb: Optional[TracebackType],
    ) -> Literal[False]:
        if self._pool is None:
            raise ValueError("threadpool does not exist")
        self._pool.shutdown(wait=True)
        return False

    def download(self) -> None:
        futs = {}
        assert self.n_items is not None
        assert self._pool is not None

        for i in range(self.n_items):
            video_id = i + 1
            downloader = MergedDownloader(video_id, self.ytd)
            logging.info("spawing merged downloader for item %d", video_id)
            fut = self._pool.submit(downloader.download_and_merge)
            futs[fut] = video_id

        for fut in as_completed(futs):
            video_id = futs[fut]
            logging.info("future for item %d complete", video_id)
            # raise exception if there was one
            fut.result()

    def _fetch_n_items(self) -> int:
        return self.ytd.num_items()


if __name__ == "__main__":  # pragma: no cover
    parser = argparse.ArgumentParser()
    parser.add_argument("-u", "--url", required=True)
    parser.add_argument("-n", "--nthreads", type=int, required=False, default=16)
    parser.add_argument("-f", "--output-format", required=False, default=None)
    parser.add_argument("-v", "--verbose", action="store_true", default=False)
    args = parser.parse_args()

    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)

    with PlaylistDownloader(
        url=args.url, nthreads=args.nthreads, output_format=args.output_format
    ) as downloader:
        downloader.download()
