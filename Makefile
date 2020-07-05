all: test types

test:
	pytest --cov yt_download_playlist --cov-report html --cov-report term testing/test_yt_download_playlist.py

types:
	mypy --strict bin/bin/yt_download_playlist.py
