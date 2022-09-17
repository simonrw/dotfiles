#!/bin/sh

set -eou pipefail

function die() {
    echo "$@" >&2
    exit 1
}

test -f Makefile || {
    die "cannot find Makefile in current directory"
}

make docs

test -d docs/_build/html || {
    die "cannot find built documentation"
}

(cd docs/_build/html && python -m http.server -b 127.0.0.1)
