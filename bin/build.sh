#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$(readlink -f "$0")")/utils.sh"

NIXARCH="$(nixarch)"

case ${NIXARCH} in
    *-linux)
        nom build .#nixosConfigurations.${HOSTTARGET}.config.system.build.toplevel $*
        ;;
    *-darwin)
        nom build ".#darwinConfigurations.${HOSTTARGET}.system" $*
        ;;
    *)
        echo "Unhandled architecture: ${NIXARCH}" >&2
        exit 1
    ;;
esac

