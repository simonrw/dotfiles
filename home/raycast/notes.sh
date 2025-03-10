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
    nWindowsOpen="$(osascript -e 'tell application "System Events" to count processes whose name is "Neovide"')"
    if [[ $nWindowsOpen = "0" ]]; then
        /Applications/Neovide.app/Contents/MacOS/neovide --frame transparent --no-tabs --fork ~/notes.md
    else
        open -a neovide
    fi
}

switchToOrStart
