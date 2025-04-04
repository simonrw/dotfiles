#!/usr/bin/env bash

set -euo pipefail

bash "$(dirname "$(readlink -f "$0")")/home-build.sh"
./result/activate
