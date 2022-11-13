#!/usr/bin/env bash

set -euxo pipefail

nix develop .#neovim-package --command nvim
