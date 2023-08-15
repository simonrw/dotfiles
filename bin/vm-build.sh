#!/usr/bin/env bash

set -euo pipefail

if [ ! -f /etc/NIXOS ]; then
    echo "This script can only be run on nixos" >&2
    exit 1
fi

# remove all existing disk images
find . -name '*.qcow2' -delete

# build the vm
nixos-rebuild build-vm --flake .#${HOSTTARGET}

echo "VM created, run ./result/bin/run-${HOSTTARGET}-vm -m 16384 -smp 4"
