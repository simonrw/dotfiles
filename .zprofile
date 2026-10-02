# Make mise available before interactive startup, including a clean login shell.
typeset -U path
[[ -d /opt/homebrew/bin ]] && path+=(/opt/homebrew/bin)
