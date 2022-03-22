if status is-interactive
    set -U fish_greeting
end

# this needs to be set for tmux to pick up
set -x SHELL (command -v fish)

# host-specific configuration
set host_config ~/.config/fish/conf.d/per-host/config.(hostname).fish
test -r $host_config; and source $host_config
set -e host_config
