#!/usr/bin/env bash

set -euxo pipefail

PACKAGES="unzip zip nodejs cargo rustc go gcc libiconv"

# set up a nix shell that's suitable for building my neovim lsp servers
nix-shell -p ${PACKAGES} --run nvim
