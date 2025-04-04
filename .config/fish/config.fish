# ~/.config/fish/config.fish: DO NOT EDIT -- this file has been generated
# automatically by home-manager.

# Only execute this file once per shell.
set -q __fish_home_manager_config_sourced; and exit
set -g __fish_home_manager_config_sourced 1

source /nix/store/4j8g164k9g5rprhcfhyjrbc7na5938vs-hm-session-vars.fish



status is-login; and begin

    # Login shell initialisation


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
    abbr --add -- nd 'nix develop --command fish'
    abbr --add -- py python
    abbr --add -- t testsearch
    abbr --add -- v vim

    # Aliases
    alias add-keys 'ssh-add (find ~/.ssh - maxdepth 1 - type f - name "id_rsa*" | grep - v pub | grep - v bak)'
    alias cat /nix/store/5m0ixbv6j0x3syzm732fplz6laig9vyk-bat-0.24.0/bin/bat
    alias clear-pycs 'find { $PWD } -name '\''*.pyc'\'' -delete'
    alias da '/nix/store/2rhpgzr5484b701il1pxs7s94vr3wlic-direnv-2.35.0/bin/direnv allow'
    alias de '/nix/store/2rhpgzr5484b701il1pxs7s94vr3wlic-direnv-2.35.0/bin/direnv edit'
    alias es 'exec $SHELL'
    alias eza 'eza --group-directories-first --header'
    alias gcpr '/nix/store/6n0vmx4j5x6rpa4n3nh79v2bw0kxv2zd-gh-2.63.0/bin/gh pr create -a @me --label '\''semver: patch'\'''
    alias gcprd '/nix/store/6n0vmx4j5x6rpa4n3nh79v2bw0kxv2zd-gh-2.63.0/bin/gh pr create -a @me --draft --label '\''semver: patch'\'''
    alias gpr '/nix/store/fmfsqbimdsw15hr4aq0di0k2ncyc043h-git-2.47.2/bin/git pull --rebase'
    alias grep /nix/store/qf82zxj5prairk2d6k2wnvxpnx2f6iwh-ripgrep-14.1.1/bin/rg
    alias la 'eza -a'
    alias less /nix/store/5m0ixbv6j0x3syzm732fplz6laig9vyk-bat-0.24.0/bin/bat
    alias ll 'eza -l'
    alias lla 'eza -la'
    alias lr thor
    alias ls eza
    alias lt 'eza --tree'
    alias more /nix/store/5m0ixbv6j0x3syzm732fplz6laig9vyk-bat-0.24.0/bin/bat
    alias nix-shell 'nix-shell --command fish'
    alias nl 'nix-locate --regex --top-level'
    alias nr 'nix repl --file '\''<nixpkgs>'\'''
    alias ns 'nix shell'
    alias ntfy notify-wrapper
    alias nvim nvim
    alias project listprojects
    alias ptl 'pytest (testsearch rerun -l)'
    alias pts 'pytest (testsearch)'
    alias pydoc 'python -m pydoc'
    alias pylab 'ipython - -pylab'
    alias sourceenv 'source ./venv/bin/activate'
    alias ta _tmux_attach
    alias thor '/nix/store/h1k0rsnsrxxypvcw36dxfryabgx4na4f-eza-0.20.24/bin/eza -s modified -l'
    alias tl tmux-last
    alias trash /nix/store/5y12dld03n0pd21bpq75z7qx9j0zwz3f-python3.12-send2trash-1.8.3/bin/send2trash
    alias tree '/nix/store/h1k0rsnsrxxypvcw36dxfryabgx4na4f-eza-0.20.24/bin/eza -T'
    alias vim nvim
    alias vimdiff 'nvim -d'
    alias vup 'nvim --headless "+Lazy! sync" +qa'
    alias watch viddy

    # Interactive shell initialisation
    /nix/store/r3gh5lpp7a0zlb9id9ys2dhf8b7fjab6-fzf-0.60.3/bin/fzf --fish | source

    set -x GLAMOUR_STYLE dark
    set -x PAGER "/nix/store/5m0ixbv6j0x3syzm732fplz6laig9vyk-bat-0.24.0/bin/bat"
    set -x PYTHONUNBUFFERED 1
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

    set -x EDITOR nvim

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
        test "$TERM_PROGRAM" != vscode
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

    # configure colour theme
    fish_config theme choose "fish default"

    function __fish_command_not_found_handler --on-event fish_command_not_found
        /nix/store/q8m2lshmi388lpwxnv7bjk5zb8xw2827-command-not-found $argv
    end

    # add completions generated by Home Manager to $fish_complete_path
    begin
        set -l joined (string join " " $fish_complete_path)
        set -l prev_joined (string replace --regex "[^\s]*generated_completions.*" "" $joined)
        set -l post_joined (string replace $prev_joined "" $joined)
        set -l prev (string split " " (string trim $prev_joined))
        set -l post (string split " " (string trim $post_joined))
        set fish_complete_path $prev "/Users/simon/.local/share/fish/home-manager_generated_completions" $post
    end

    /nix/store/2rhpgzr5484b701il1pxs7s94vr3wlic-direnv-2.35.0/bin/direnv hook fish | source


end
