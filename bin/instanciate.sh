#!/usr/bin/env bash

set -euo pipefail

BINDIR="$(dirname "$(readlink -f "$0")")"
. "${BINDIR}/utils.sh"

NIXARCH="$(nixarch)"

case ${NIXARCH} in
    *-linux)
        echo "Not implemented for ${NIXARCH}" >&2
        exit 1
        ;;
    *-darwin)
        echo "Checking system configuration" >&2
        nix eval --raw ".#darwinConfigurations.${HOSTTARGET}.system" $*
        ;;
esac
