#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$(readlink -f "$0")")/utils.sh"

NIXARCH="$(nixarch)"

case ${NIXARCH} in
    *-linux)
        sudo nixos-rebuild switch --flake . $*
        ;;
    *-darwin)
        ./bin/build.sh
        ./result/activate-user
        sudo ./result/activate
        ;;
    *)
        echo "Unhandled architecture: ${NIXARCH}" >&2
        exit 1
    ;;
esac
