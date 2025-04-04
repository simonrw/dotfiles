#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$(readlink -f "$0")")/utils.sh"

if [ ! -f /etc/NIXOS ]; then
    echo "This script can only be run on nixos" >&2
    exit 1
fi

# remove all existing disk images
find . -name '*.qcow2' -delete

# build the vm
nixos-rebuild build-vm --flake .#$(get-hosttarget)

echo "VM created, run ./result/bin/run-$(get-hosttarget)-vm -m 16384 -smp 4"
