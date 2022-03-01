if status is-interactive
    set -U fish_greeting
end

# this needs to be set for tmux to pick up
set -x SHELL (command -v fish)
