#!/usr/bin/env bash

set -e

main() {
    if [ -z "$1" ]; then
        echo "Program usage: <infile> [output] [args]" >&2
        exit 1
    fi

    if [ -z "$2" ]; then
        echo "No output specified, printing to stdout" >&2
        ipython nbconvert "$1" --ExecutePreprocessor.enabled=True --to notebook --stdout "${@:3}"
    else
        ipython nbconvert "$1" --ExecutePreprocessor.enabled=True --to notebook --output "$2" "${@:3}"
    fi
}

main "$@"
