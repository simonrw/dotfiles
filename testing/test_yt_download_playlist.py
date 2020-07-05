import sys
from unittest import mock

sys.path.append("bin/bin")
import pytest
import yt_download_playlist as ydp


class BadRetry:
    def __init__(self, raise_error):
        self.calls = 0
        self.raise_error = raise_error

    def __call__(self):
        self.calls += 1
        if self.raise_error:
            raise ValueError("bad")


class TestBackoff:
    def test_retries(self):

        r = BadRetry(raise_error=True)
        backoff = ydp.ExponentialBackoff(r)

        # reduce the factor
        backoff._sleep_time = 0.001

        with pytest.raises(ValueError):
            backoff.run()

        assert r.calls == 6

    def test_ok_case(self):
        r = BadRetry(raise_error=False)
        backoff = ydp.ExponentialBackoff(r)

        # reduce the factor
        backoff._sleep_time = 0.001

        backoff.run()

        assert r.calls == 1


class TestYoutubeDownload:
    def test_get_filename(self):
        ytd = ydp.YoutubeDownload(url="https://example.com")
        with mock.patch("yt_download_playlist.sp") as mock_sp:
            mock_sp.check_output.return_value = b"ok"
            filename = ytd.get_filename(file_format=100, playlist_item=1)

        assert filename == "ok"
        calls = mock_sp.check_output.call_args_list
        assert len(calls) == 1
        args, kwargs = calls[0]
        cmd = args[0]
        assert "100" in cmd
        assert "1" in cmd
        assert "https://example.com" in cmd
        assert "--get-filename" in cmd

    def test_download(self):
        ytd = ydp.YoutubeDownload(url="https://example.com")
        with mock.patch("yt_download_playlist.sp") as mock_sp:
            ytd.download(file_format=100, playlist_item=1)

        calls = mock_sp.check_call.call_args_list
        assert len(calls) == 1
        args, kwargs = calls[0]
        cmd = args[0]

        assert "https://example.com" in cmd
        assert "1" in cmd
        assert "100" in cmd
    
    def test_num_items(self):
        ytd = ydp.YoutubeDownload(url="https://example.com")
        with mock.patch("yt_download_playlist.sp") as mock_sp:
            mock_sp.check_output.return_value = "\n".join([
                "{}",
                "{}",
                "{}",
                ]).encode()
            num_items = ytd.num_items()

        assert num_items == 3

        calls = mock_sp.check_output.call_args_list
        assert len(calls) == 1
        args, kwargs = calls[0]
        cmd = args[0]

        assert "https://example.com" in cmd
        assert "--flat-playlist" in cmd
        assert "--dump-json" in cmd
