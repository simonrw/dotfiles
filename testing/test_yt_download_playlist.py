import sys

sys.path.append("bin/bin")
import pytest
from yt_download_playlist import ExponentialBackoff


class TestBackoff:
    def test_retries(self):
        class BadRetry:
            def __init__(self):
                self.calls = 0

            def __call__(self):
                self.calls += 1
                raise ValueError("bad")

        r = BadRetry()
        backoff = ExponentialBackoff(r)

        # reduce the factor
        backoff._sleep_time = 0.001

        with pytest.raises(ValueError):
            backoff.run()

        assert r.calls == 6
