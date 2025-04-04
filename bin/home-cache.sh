#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$(readlink -f "$0")")/utils.sh"

./bin/home-build.sh --json \
  | jq -r '.[].outputs | to_entries[].value' \
  | cachix push mindriot101-home
