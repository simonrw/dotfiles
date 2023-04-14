#!/usr/bin/env bash

# Run a nix repl with the current nixos configuration loaded

nix repl --extra-experimental-features "flakes repl-flake" .#nixosConfigurations.$(hostname -s) $*


