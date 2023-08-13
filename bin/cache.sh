#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$(readlink -f "$0")")/utils.sh"
NIXARCH="$(nixarch)"
HOSTNAME="${HOSTNAME:-$(hostname -s)}"

case ${NIXARCH} in
    *-linux)
        echo "Not implemented" >&2
        exit 1
        ;;
    *-darwin)
        nix --extra-experimental-features "nix-command flakes" build ".#darwinConfigurations.${HOSTNAME}.system" --json \
            | jq -r '.[].outputs | to_entries[].value' \
            | cachix push mindriot101-home
        ;;
esac
