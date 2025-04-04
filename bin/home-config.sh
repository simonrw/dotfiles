#!/usr/bin/env bash

# Run a nix repl with the current home-manager configuration loaded

nix repl --extra-experimental-features "flakes repl-flake" .#homeConfigurations.x86_64-linux.simon


