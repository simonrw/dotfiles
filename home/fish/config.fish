set -x SHELL (command -v fish)

set -x BUILD_PREFIX {$HOME}/.local
set -x PATH {$BUILD_PREFIX}/bin {$HOME}/.bin {$HOME}/.poetry/bin /usr/local/bin {$HOME}/.cargo/bin {$HOME}/bin {$GOPATH}/bin {$HOME}/.npm-packages/bin {$PATH}
set -x NODE_PATH {$HOME}/.npm-packages/lib/node_modules
set -x GOPATH {$HOME}/dev/gocode
set -x REVIEW_BASE main
set -x lC_CTYPE en_GB.UTF-8
set -x LC_ALL en_GB.UTF-8
set -x LANG en_GB.UTF-8
set -x CARGO_TARGET_DIR {$HOME}/.cargo-target
set -x NIXPKGS_ALLOW_UNFREE 1
set -x NTFY_TOPIC simonrw-notify

# centralise where python puts its .pyc files
set -x PYTHONPYCACHEPREFIX {$HOME}/.python-cache

set -x EDITOR "nvim"

# host-specific configuration
set host_config ~/.config/fish/conf.d/per-host/config.(hostname).fish
test -r $host_config; and source $host_config
set -e host_config

function fs
    tmux-session-history
end

# tmux configuration
fish_ssh_agent

switch (uname)
    case Darwin
        # set -x DYLD_LIBRARY_PATH {$BUILD_PREFIX}/lib {$DYLD_LIBRARY_PATH}
        #
        # fix nix path
        set -x PATH /etc/profiles/per-user/(whoami)/bin {$PATH} /opt/homebrew/bin
    case '*'
end

# fix gpg tty
if isatty
set -x GPG_TTY (tty)
end

# wrap tflocal completion
complete --command tflocal --wraps terraform

# wrap nix completion
complete --command nom --wraps nix

# if a local configuration override file exists, source it
if test -f $HOME/.config/fish/local.fish;
source $HOME/.config/fish/local.fish
end

