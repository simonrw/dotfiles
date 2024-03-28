#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$(readlink -f "$0")")/utils.sh"

NIXARCH="$(nixarch)"

case ${NIXARCH} in
    *-linux)
        sudo nixos-rebuild switch --flake . $*
        ;;
    *-darwin)
        notify-wrapper -m "Nix build complete" ./bin/build.sh
        sudo ./result/activate
        ./result/activate-user
        ;;
    *)
        echo "Unhandled architecture: ${NIXARCH}" >&2
        exit 1
    ;;
esac
