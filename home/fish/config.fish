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
set -x DOCKER_HOST unix:///$HOME/.config/colima/default/docker.sock

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
function _not_inside_tmux
    test -z "$TMUX"
end

function _not_inside_neovim
    test -z "$NVIM"
end

function _not_inside_emacs
    test -z "$INSIDE_EMACS" && test -z "$EMACS"
end

function _not_inside_vscode_term
    test "$TERM_PROGRAM" != "vscode"
end

function _not_inside_zellij
    test -z "$ZELLIJ_SESSION_NAME"
end

function _not_inside_pycharm
    test -z "$INSIDE_PYCHARM"
end

function _inside_x_session
    switch (uname)
        case Linux
            ps aux | grep -q -i xorg
        case '*'
            return 0
    end
end

function _not_disabled
    test -z "$TMUX_DISABLED"
end

function ensure_tmux_is_running
    if _not_disabled && _not_inside_tmux && _not_inside_neovim && _not_inside_emacs && _inside_x_session && _not_inside_vscode_term && _not_inside_zellij && _not_inside_pycharm
        tat
    end
end

fish_ssh_agent

ensure_tmux_is_running

switch (uname)
    case Darwin
        # set -x DYLD_LIBRARY_PATH {$BUILD_PREFIX}/lib {$DYLD_LIBRARY_PATH}
        #
        # fix nix path
        set -x PATH /etc/profiles/per-user/(whoami)/bin "/Applications/PyCharm CE.app/Contents/MacOS" {$PATH}
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

# wrap awslocal completion
complete --command awslocal --wraps aws

# wrap laws completion
complete --command laws --wraps aws

# ghostty completion (macos)
if test -f /Applications/Ghostty.app/Contents/Resources/fish/vendor_completions.d/ghostty.fish;
    source /Applications/Ghostty.app/Contents/Resources/fish/vendor_completions.d/ghostty.fish
end

# if a local configuration override file exists, source it
if test -f $HOME/.config/fish/local.fish;
source $HOME/.config/fish/local.fish
end

