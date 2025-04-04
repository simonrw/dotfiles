#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$(readlink -f "$0")")/utils.sh"

NIXARCH="$(nixarch)"

nix eval --raw ".#homeConfigurations.${NIXARCH}.${USER}.activationPackage" $*
