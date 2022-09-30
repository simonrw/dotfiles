if status is-interactive
    set -U fish_greeting
end

# this needs to be set for tmux to pick up
set -x SHELL (command -v fish)

# host-specific configuration
set host_config ~/.config/fish/conf.d/per-host/config.(hostname).fish
test -r $host_config; and source $host_config
set -e host_config

# some key variables
set -x GOPATH {$HOME}/dev/gocode
set -x PATH {$PATH} {$GOPATH}/bin
set -x PYTHONBREAKPOINT pudb.set_trace

# include foreign env
set fish_function_path $fish_function_path {$HOME}/dotfiles/external/plugin-foreign-env/functions

# set up nix
test -f {$HOME}/.nix-profile/etc/profile.d/nix.sh; and fenv source {$HOME}/.nix-profile/etc/profile.d/nix.sh
