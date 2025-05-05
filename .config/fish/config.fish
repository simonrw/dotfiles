# ~/.config/fish/config.fish: DO NOT EDIT -- this file has been generated
# automatically by home-manager.

# Only execute this file once per shell.
set -q __fish_already_sourced; and exit
set -g __fish_already_sourced 1

# set up environment variables
set -gx FZF_CTRL_T_COMMAND 'fd --no-ignore --hidden --type f'
set -gx FZF_CTRL_T_OPTS '--preview \'bat --color always {}\''
set -gx FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow -g ''!{.git,venv,node_modules}/*'' 2> /dev/null'
set -gx FZF_DEFAULT_OPTS '--tiebreak begin --ansi --no-mouse --tabstop 4 --inline-info --color dark'
set -gx GNUPGHOME "$HOME/.gnupg"
set -gx JQ_COLORS '1;30:0;37:0;37:0;37:0;32:1;37:1;37'
set -gx TMUX_TMPDIR (test -n "$XDG_RUNTIME_DIR" && echo "$XDG_RUNTIME_DIR" || echo '/run/user/'(id -u | string collect; or echo))
set -gx WEBKIT_DISABLE_COMPOSITING_MODE '1'
set -gx XDG_CACHE_HOME "$HOME/.cache"
set -gx XDG_CONFIG_HOME "$HOME/.config"
set -gx XDG_DATA_HOME "$HOME/.local/share"
set -gx XDG_STATE_HOME "$HOME/.local/state"



status is-login; and begin

    # Login shell initialisation
    set -x PATH /opt/homebrew/opt/curl/bin /opt/homebrew/opt/make/libexec/gnubin /opt/homebrew/opt/coreutils/libexec/gnubin /opt/homebrew/bin /opt/homebrew/opt/sqlite/bin {$PATH}
end

status is-interactive; and begin

    # Abbreviations
    abbr --add -- c cargo
    abbr --add -- d dev
    abbr --add -- g git
    abbr --add -- gco 'git checkout'
    abbr --add -- gp 'git pull'
    abbr --add -- ipy ipython
    abbr --add -- k kubectl
    abbr --add -- py python
    abbr --add -- t testsearch
    abbr --add -- v vim

    # Aliases
    alias add-keys 'ssh-add (find ~/.ssh - maxdepth 1 - type f - name "id_rsa*" | grep - v pub | grep - v bak)'
    alias cat bat
    alias clear-pycs 'find { $PWD } -name '\''*.pyc'\'' -delete'
    alias es 'exec $SHELL'
    alias eza 'eza --group-directories-first --header'
    alias gcpr 'gh pr create -a @me --label '\''semver: patch'\'''
    alias gcprd 'gh pr create -a @me --draft --label '\''semver: patch'\'''
    alias gpr 'git pull --rebase'
    alias grep rg
    alias la 'eza -a'
    alias less bat
    alias ll 'eza -l'
    alias lla 'eza -la'
    alias lr thor
    alias ls eza
    alias lt 'eza --tree'
    alias more bat
    alias ntfy notify-wrapper
    alias nvim nvim
    alias project listprojects
    alias ptl 'pytest (testsearch rerun -l)'
    alias pts 'pytest (testsearch)'
    alias pydoc 'python -m pydoc'
    alias pylab 'ipython - -pylab'
    alias sourceenv 'source ./venv/bin/activate'
    alias ta _tmux_attach
    alias thor 'eza -s modified -l'
    alias tl tmux-last
    alias trash send2trash
    alias tree 'eza -T'
    alias vim nvim
    alias vimdiff 'nvim -d'
    alias vup 'nvim --headless "+Lazy! sync" +qa'
    alias watch viddy

    # Interactive shell initialisation
    set -x GLAMOUR_STYLE dark
    set -x PAGER bat
    set -x PYTHONUNBUFFERED 1
    set -x SHELL (command -v fish)
    set -x MANPAGER "nvim +Man!"

    set -x BUILD_PREFIX {$HOME}/.local
    set -x PATH {$BUILD_PREFIX}/bin {$HOME}/.bin  /usr/local/bin {$HOME}/.cargo/bin {$HOME}/bin {$GOPATH}/bin {$HOME}/.npm-packages/bin {$PATH}
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

    set -x EDITOR nvim

    # set up fzf
    fzf --fish | source


    # host-specific configuration
    set host_config ~/.config/fish/conf.d/per-host/config.(hostname).fish
    test -r $host_config; and source $host_config
    set -e host_config

    function fs
        tmux-session-history
    end

    # fix gpg tty
    if isatty
        set -x GPG_TTY (tty)
    end

    # wrap tflocal completion
    complete --command tflocal --wraps terraform

    # wrap awslocal completion
    complete --command awslocal --wraps aws

    # ghostty completion (macos)
    if test -f /Applications/Ghostty.app/Contents/Resources/fish/vendor_completions.d/ghostty.fish
        source /Applications/Ghostty.app/Contents/Resources/fish/vendor_completions.d/ghostty.fish
    end

    # mise integration
    if status is-interactive
        mise activate fish | source
    else
        mise activate fish --shims | source
    end

    # if a local configuration override file exists, source it
    if test -f $HOME/.config/fish/local.fish
        source $HOME/.config/fish/local.fish
    end


    set -x EDITOR nvim

    # set the theme
    fish_config theme choose "Catppuccin Mocha"
end
