set -gx LANG en_GB.UTF-8
set -gx LC_ALL en_GB.UTF-8
set -gx LC_CTYPE en_GB.UTF-8

# Dark/light theme detection
if $HOME/.bin/is-dark-theme 2>/dev/null
    set -gx __IS_DARK_THEME 1
else
    set -gx __IS_DARK_THEME 0
end

# FZF
set -gx FZF_CTRL_T_COMMAND 'fd --no-ignore --hidden --type f'
set -gx FZF_DEFAULT_COMMAND "rg --files --no-ignore --hidden --follow -g '!{.git,venv,node_modules}/*' 2>/dev/null"
if test "$__IS_DARK_THEME" = 1
    set -gx FZF_DEFAULT_OPTS '--tiebreak begin --ansi --no-mouse --tabstop 4 --inline-info --color dark'
else
    set -gx FZF_DEFAULT_OPTS '--tiebreak begin --ansi --no-mouse --tabstop 4 --inline-info --color light'
end

set -gx GNUPGHOME $HOME/.gnupg
set -gx JQ_COLORS '1;30:0;37:0;37:0;37:0;32:1;37:1;37'
set -gx XDG_CACHE_HOME $HOME/.cache
set -gx XDG_CONFIG_HOME $HOME/.config
set -gx XDG_DATA_HOME $HOME/.local/share
set -gx XDG_STATE_HOME $HOME/.local/state

if test "$__IS_DARK_THEME" = 1
    set -gx BAT_CONFIG_PATH $HOME/.config/bat/dark/config
    set -gx GLAMOUR_STYLE dark
    set -gx K9S_SKIN catppuccin-mocha
else
    set -gx BAT_CONFIG_PATH $HOME/.config/bat/light/config
    set -gx GLAMOUR_STYLE light
    set -gx K9S_SKIN catppuccin-latte
end

set -gx PAGER bat
set -gx PYTHONUNBUFFERED 1
set -gx MANPAGER "nvim +Man!"
set -gx MANPATH /opt/homebrew/share/man $MANPATH

set -gx BUILD_PREFIX $HOME/.local
set -gx GOPATH $HOME/dev/gocode
set -gx REVIEW_BASE main
set -gx NIXPKGS_ALLOW_UNFREE 1
set -gx NTFY_TOPIC simonrw-notify
set -gx NODE_PATH $HOME/.npm-packages/lib/node_modules
set -gx NODE_COMPILE_CACHE $HOME/.cache/nodejs-compile-cache
set -gx CARGO_TARGET_DIR $HOME/.cargo-target
set -gx PYTHONPYCACHEPREFIX $HOME/.python-cache
set -gx EDITOR nvim
set -gx MISE_PIPX_UVX true
set -gx HOMEBREW_NO_ANALYTICS 1
set -gx HOMEBREW_NO_AUTO_UPDATE 1

# PATH (highest priority first)
fish_add_path /opt/homebrew/opt/gnu-sed/libexec/gnubin
fish_add_path $BUILD_PREFIX/bin
fish_add_path $HOME/.bin
fish_add_path $HOME/.local/share/bob/nvim-bin
fish_add_path /usr/local/bin
fish_add_path $HOME/.cargo/bin
fish_add_path $HOME/bin
fish_add_path $GOPATH/bin
fish_add_path $HOME/.npm-packages/bin
fish_add_path /opt/homebrew/bin
fish_add_path /opt/homebrew/sbin
fish_add_path --append $HOME/Applications/PyCharm.app/Contents/MacOS
fish_add_path --append /opt/homebrew/opt/grep/libexec/gnubin
fish_add_path --append /opt/homebrew/opt/curl/bin
fish_add_path --append /opt/homebrew/opt/make/libexec/gnubin
fish_add_path --append /opt/homebrew/opt/coreutils/libexec/gnubin
fish_add_path --append /opt/homebrew/opt/sqlite/bin
fish_add_path --append $HOME/.rd/bin
fish_add_path --append $HOME/.antigravity/antigravity/bin

# GPG TTY
if isatty
    set -gx GPG_TTY (tty)
end

# SSH agent
if test -f ~/.ssh/agent.fish
    source ~/.ssh/agent.fish
end
if not set -q SSH_AGENT_PID; or not kill -0 $SSH_AGENT_PID 2>/dev/null
    ssh-agent -c | string replace 'setenv' 'set -gx' | string replace ';' '' | source
    printf "set -gx SSH_AUTH_SOCK %s\nset -gx SSH_AGENT_PID %s\n" $SSH_AUTH_SOCK $SSH_AGENT_PID >~/.ssh/agent.fish
end
