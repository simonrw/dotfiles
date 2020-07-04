import sys

sys.path.append("bin/bin")
import pytest
from yt_download_playlist import ExponentialBackoff


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
        backoff = ExponentialBackoff(r)

        # reduce the factor
        backoff._sleep_time = 0.001

        with pytest.raises(ValueError):
            backoff.run()

        assert r.calls == 6

    def test_ok_case(self):
        r = BadRetry(raise_error=False)
        backoff = ExponentialBackoff(r)

        # reduce the factor
        backoff._sleep_time = 0.001

        backoff.run()

        assert r.calls == 1
