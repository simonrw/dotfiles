function __is_dark_theme() {
    defaults read -g AppleInterfaceStyle >/dev/null 2>&1
}

if __is_dark_theme; then
    export __IS_DARK_THEME=1
else
    export __IS_DARK_THEME=0
fi

# only source this file once per shell
if [[ "$__ZSH_ALREADY_SOURCED" == "1" ]]; then
    exit
fi
export __ZSH_ALREADY_SOURCED=1

# set up environment variables
export FZF_CTRL_T_COMMAND='fd --no-ignore --hidden --type f'
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g ''!{.git,venv,node_modules}/*'' 2> /dev/null'
if [[ "$__IS_DARK_THEME" == "1" ]]; then
    export FZF_DEFAULT_OPTS='--tiebreak begin --ansi --no-mouse --tabstop 4 --inline-info --color dark'
else
    export FZF_DEFAULT_OPTS='--tiebreak begin --ansi --no-mouse --tabstop 4 --inline-info --color light'
fi
export GNUPGHOME="$HOME/.gnupg"
export JQ_COLORS='1;30:0;37:0;37:0;37:0;32:1;37:1;37'
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

if [[ "$__IS_DARK_THEME" == "1" ]]; then
    export BAT_CONFIG_PATH="$HOME/.config/bat/dark/config"
    export GLAMOUR_STYLE=dark
else
    export BAT_CONFIG_PATH="$HOME/.config/bat/light/config"
    export GLAMOUR_STYLE=light
fi


export PAGER=bat
export PYTHONUNBUFFERED=1
export SHELL=$(command -v fish)
export MANPAGER="nvim +Man!"
export MANPATH=/opt/homebrew/share/man:$MANPATH

export BUILD_PREFIX=${HOME}/.local
export GOPATH=${HOME}/dev/gocode
export PATH=${BUILD_PREFIX}/bin:${HOME}/.bin:/usr/local/bin:${HOME}/.cargo/bin:${HOME}/bin:${GOPATH}/bin:${HOME}/.npm-packages/bin:${PATH}
export REVIEW_BASE=main
export lC_CTYPE=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8
export LANG=en_GB.UTF-8
export NIXPKGS_ALLOW_UNFREE=1
export NTFY_TOPIC=simonrw-notify

# centralise where python puts its .pyc files
export PYTHONPYCACHEPREFIX=${HOME}/.python-cache
export EDITOR=nvim

function isatty() {
    [ -t 0 ] && [ -t 1 ]
}
if isatty; then
    export GPG_TTY=$(tty)
fi
