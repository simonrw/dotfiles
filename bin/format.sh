#!/usr/bin/env bash

set -euo pipefail

fd '.*\.nix$' | xargs nix run nixpkgs#alejandra
