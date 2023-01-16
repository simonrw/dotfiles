#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$(readlink -f "$0")")/utils.sh"

NIXARCH="$(nixarch)"
HOSTNAME="$(hostname -s)"

case ${NIXARCH} in
    *-linux)
        nixos-rebuild build --flake .
        ;;
    *-darwin)
        nix --extra-experimental-features "nix-command flakes" build ".#darwinConfigurations.${HOSTNAME}.system"
        ./result/sw/bin/darwin-rebuild build --flake ".#${HOSTNAME}" "$@"
        ;;
    *)
        echo "Unhandled architecture: ${NIXARCH}" >&2
        exit 1
    ;;
esac

