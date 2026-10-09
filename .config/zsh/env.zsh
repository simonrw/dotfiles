export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8
export LC_CTYPE=en_GB.UTF-8

if is-dark-theme; then
    export __IS_DARK_THEME=1
else
    export __IS_DARK_THEME=0
fi

export FZF_CTRL_T_COMMAND='fd --no-ignore --hidden --type f'
export FZF_DEFAULT_COMMAND="rg --files --no-ignore --hidden --follow -g '!{.git,venv,node_modules}/*' 2>/dev/null"
if [[ "$__IS_DARK_THEME" == 1 ]]; then
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
export OPENSPEC_TELEMETRY=0

if [[ "$__IS_DARK_THEME" == 1 ]]; then
    export BAT_CONFIG_PATH="$HOME/.config/bat/dark/config"
    export GLAMOUR_STYLE=dark
    export K9S_SKIN=catppuccin-macchiato
else
    export BAT_CONFIG_PATH="$HOME/.config/bat/light/config"
    export GLAMOUR_STYLE=light
    export K9S_SKIN=catppuccin-latte
fi

export PAGER=bat
export PYTHONUNBUFFERED=1
export MANPAGER="nvim +Man!"
export MANPATH="/opt/homebrew/share/man${MANPATH:+:$MANPATH}"
export BUILD_PREFIX="$HOME/.local"
export GOPATH="$HOME/dev/gocode"
export REVIEW_BASE=main
export SESSION_BACKEND=rex
export NIXPKGS_ALLOW_UNFREE=1
export NTFY_TOPIC=simonrw-notify
export NTFY_DEFAULT_TOPIC="$NTFY_TOPIC"
export NODE_PATH="$HOME/.npm-packages/lib/node_modules"
export NODE_COMPILE_CACHE="$HOME/.cache/nodejs-compile-cache"
export PYTHONPYCACHEPREFIX="$HOME/.python-cache"
export EDITOR=nvim
export MISE_PIPX_UVX=true
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_AUTO_UPDATE=1
export CLAUDE_CODE_NO_FLICKER=1
export CLAUDE_MONITOR_URL=https://csm.tortoise-bearded.ts.net

# fish_add_path prepends each entry in turn, only when the directory exists.
typeset -U path
typeset -a __zsh_user_paths=()
for directory in \
    /opt/homebrew/opt/gnu-sed/libexec/gnubin \
    "$BUILD_PREFIX/bin" "$HOME/.bin" "$HOME/.local/share/bob/nvim-bin" \
    /usr/local/bin "$HOME/.cargo/bin" "$HOME/bin" "$GOPATH/bin" \
    "$HOME/.npm-packages/bin" /opt/homebrew/bin /opt/homebrew/sbin; do
    [[ -d "$directory" ]] && __zsh_user_paths=("$directory" "${__zsh_user_paths[@]}")
done
for directory in \
    "$HOME/Applications/PyCharm.app/Contents/MacOS" \
    /opt/homebrew/opt/grep/libexec/gnubin /opt/homebrew/opt/curl/bin \
    /opt/homebrew/opt/make/libexec/gnubin /opt/homebrew/opt/coreutils/libexec/gnubin \
    /opt/homebrew/opt/sqlite/bin "$HOME/.rd/bin" "$HOME/.config/emacs/bin" \
    "$HOME/.antigravity/antigravity/bin"; do
    [[ -d "$directory" ]] && __zsh_user_paths+=("$directory")
done
path=("${__zsh_user_paths[@]}" "$path[@]")
unset directory __zsh_user_paths

[[ -n "${SSH_CONNECTION+x}" ]] && export OP_BIOMETRIC_UNLOCK_ENABLED=false
[[ -t 0 ]] && export GPG_TTY=$(tty)

# Reuse Fish's agent so switching shells does not start a second one.
__zsh_load_ssh_agent_env() {
    [[ -f "$HOME/.ssh/agent.fish" ]] || return 1
    local directive flag name value
    while read -r directive flag name value; do
        [[ "$directive $flag" == 'set -gx' ]] || continue
        case "$name" in
            SSH_AUTH_SOCK|SSH_AGENT_PID) export "$name=$value" ;;
        esac
    done < "$HOME/.ssh/agent.fish"
    [[ -n "$SSH_AGENT_PID" ]] && kill -0 "$SSH_AGENT_PID" 2>/dev/null
}

__zsh_start_ssh_agent() {
    mkdir -p "$HOME/.ssh/agent" || return
    local agent_env
    agent_env=$(ssh-agent -s) || return
    eval "$agent_env" >/dev/null || return
    printf 'set -gx SSH_AUTH_SOCK %s\nset -gx SSH_AGENT_PID %s\n' \
        "$SSH_AUTH_SOCK" "$SSH_AGENT_PID" > "$HOME/.ssh/agent.fish"
}

if ! __zsh_load_ssh_agent_env; then
    __zsh_start_ssh_agent
    unset SSH_AUTH_SOCK SSH_AGENT_PID
fi
