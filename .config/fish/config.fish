# ~/.config/fish/config.fish: DO NOT EDIT -- this file has been generated
# automatically by home-manager.

# Only execute this file once per shell.
set -q __fish_already_sourced; and exit
set -g __fish_already_sourced 1

# set up environment variables
set -gx DIRENV_LOG_FORMAT ''
set -gx FZF_CTRL_T_COMMAND 'fd --no-ignore --type f'
set -gx FZF_CTRL_T_OPTS '--preview \'bat --color always {}\''
set -gx FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow -g ''!{.git,venv,node_modules}/*'' 2> /dev/null'
set -gx FZF_DEFAULT_OPTS '--tiebreak begin --ansi --no-mouse --tabstop 4 --inline-info --color dark'
set -gx GNUPGHOME '/Users/simon/.gnupg'
set -gx JQ_COLORS '1;30:0;37:0;37:0;37:0;32:1;37:1;37'
set -gx TMUX_TMPDIR (test -n "$XDG_RUNTIME_DIR" && echo "$XDG_RUNTIME_DIR" || echo '/run/user/'(id -u | string collect; or echo))
set -gx WEBKIT_DISABLE_COMPOSITING_MODE '1'
set -gx XDG_CACHE_HOME '/Users/simon/.cache'
set -gx XDG_CONFIG_HOME '/Users/simon/.config'
set -gx XDG_DATA_HOME '/Users/simon/.local/share'
set -gx XDG_STATE_HOME '/Users/simon/.local/state'



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
    abbr --add -- py python
    abbr --add -- t testsearch
    abbr --add -- v vim

    # Aliases
    alias add-keys 'ssh-add (find ~/.ssh - maxdepth 1 - type f - name "id_rsa*" | grep - v pub | grep - v bak)'
    alias cat bat
    alias clear-pycs 'find { $PWD } -name '\''*.pyc'\'' -delete'
    alias da 'direnv allow'
    alias de 'direnv edit'
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
    fzf --fish | source

    set -x GLAMOUR_STYLE dark
    set -x PAGER bat
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

    # fix gpg tty
    if isatty
        set -x GPG_TTY (tty)
    end

    # wrap tflocal completion
    complete --command tflocal --wraps terraform

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

    # add completions generated by Home Manager to $fish_complete_path
    begin
        set -l joined (string join " " $fish_complete_path)
        set -l prev_joined (string replace --regex "[^\s]*generated_completions.*" "" $joined)
        set -l post_joined (string replace $prev_joined "" $joined)
        set -l prev (string split " " (string trim $prev_joined))
        set -l post (string split " " (string trim $post_joined))
        set fish_complete_path $prev "/Users/simon/.local/share/fish/home-manager_generated_completions" $post
    end

    direnv hook fish | source
end
