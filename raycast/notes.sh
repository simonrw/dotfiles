#!/bin/bash

set -euo pipefail

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Notes
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 📝
# @raycast.packageName Notes

# Documentation:
# @raycast.description Open my notes
# @raycast.author Simon Walker

function switchToOrStart() {
    open -a Emacs ~/notes.org
}

switchToOrStart
