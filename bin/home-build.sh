#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$(readlink -f "$0")")/utils.sh"

NIXARCH="$(nixarch)"

nom build ".#homeConfigurations.${NIXARCH}.${USER}.activationPackage" $*
