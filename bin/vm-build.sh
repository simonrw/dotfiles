#!/usr/bin/env bash

set -euo pipefail

if [ ! -f /etc/NIXOS ]; then
    echo "This script can only be run on nixos" >&2
    exit 1
fi

HOSTNAME="$(hostname -s)"

# remove all existing disk images
find . -name '*.qcow2' -delete

# build the vm
nixos-rebuild build-vm --flake .#${HOSTNAME}

echo "VM created, run ./result/bin/run-${HOSTNAME}-vm -m 16384 -smp 4"
