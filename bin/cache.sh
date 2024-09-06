#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$(readlink -f "$0")")/utils.sh"
NIXARCH="$(nixarch)"
NIX=${nom:-nix}

case ${NIXARCH} in
    *-linux)
        echo "Not implemented" >&2
        exit 1
        ;;
    *-darwin)
        ${NIX} --extra-experimental-features "nix-command flakes" build ".#darwinConfigurations.${HOSTTARGET}.system" --json \
            | jq -r '.[].outputs | to_entries[].value' \
            | cachix push mindriot101-home
        ;;
esac
