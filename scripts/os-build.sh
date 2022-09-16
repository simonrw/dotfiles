#!/usr/bin/env bash

set -euo pipefail

. "$(dirname $(readlink -f $0))/utils.sh"

NIXARCH="$(nixarch)"
HOSTNAME="$(hostname -s)"

nix --extra-experimental-features "nix-command flakes" build ".#darwinConfigurations.${NIXARCH}.${HOSTNAME}.system"
./result/sw/bin/darwin-rebuild build --flake ".#${NIXARCH}.${HOSTNAME}" $*

