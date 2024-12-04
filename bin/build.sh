#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$(readlink -f "$0")")/utils.sh"

NIXARCH="$(nixarch)"
NIX=${NIX:-nix}

case ${NIXARCH} in
    *-linux)
        ${NIX} build .#nixosConfigurations.${HOSTTARGET}.config.system.build.toplevel $*
        ;;
    *-darwin)
        ${NIX} build ".#darwinConfigurations.${HOSTTARGET}.system" $*
        ;;
    *)
        echo "Unhandled architecture: ${NIXARCH}" >&2
        exit 1
    ;;
esac

