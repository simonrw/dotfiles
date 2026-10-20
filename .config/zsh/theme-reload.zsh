# A FIFO wakes ZLE without injecting keystrokes or signalling an unrelated process.
# While a command runs, the request waits until the shell returns to its prompt.
[[ -o interactive && -n "$TMUX" && -n "$TMUX_PANE" ]] || return
(( ${+__zsh_theme_fd} )) && return

__zsh_reload_theme() {
    local request pending
    while read -r -t 0 -u "$__zsh_theme_fd" request; do
        pending=1
    done
    [[ -n "$pending" ]] || return 0

    source "$HOME/.config/zsh/theme-env.zsh"
    source "$HOME/.config/zsh/theme.zsh"
    if zle; then
        # Invalidate cached syntax colours even though BUFFER hasn't changed.
        unset _ZSH_HIGHLIGHT_PRIOR_BUFFER
        (( ${+functions[_zsh_highlight]} )) && _zsh_highlight
        if (( ${+functions[_zsh_autosuggest_highlight_reset]} )); then
            _zsh_autosuggest_highlight_reset
            _zsh_autosuggest_highlight_apply
        fi
        zle -R
    fi
}

__zsh_theme_prompt() {
    # A nested shell may have received changes while this shell was waiting.
    local previous_reader
    previous_reader=$(command tmux show-option -pqv -t "$TMUX_PANE" @theme_reload_fifo \
        ';' set-option -pq -t "$TMUX_PANE" @theme_reload_fifo "$__zsh_theme_dir/notify" 2>/dev/null)
    if [[ "$previous_reader" != "$__zsh_theme_dir/notify" ]]; then
        source "$HOME/.config/zsh/theme-env.zsh"
        source "$HOME/.config/zsh/theme.zsh"
    fi
    __zsh_reload_theme
}

__zsh_theme_cleanup() {
    zle -F "$__zsh_theme_fd"
    exec {__zsh_theme_fd}>&-
    command rm -f -- "$__zsh_theme_dir/notify"
    command rmdir -- "$__zsh_theme_dir"
}

zmodload zsh/system
typeset -g __zsh_theme_dir
__zsh_theme_dir=$(mktemp -d "${TMPDIR:-/tmp}/zsh-theme.XXXXXXXX") || return
command mkfifo -m 600 "$__zsh_theme_dir/notify" || {
    command rmdir -- "$__zsh_theme_dir"
    return 1
}
# CLOEXEC prevents external commands from keeping the pipe alive after zsh exits.
sysopen -r -w -o cloexec -u __zsh_theme_fd "$__zsh_theme_dir/notify" || {
    command rm -f -- "$__zsh_theme_dir/notify"
    command rmdir -- "$__zsh_theme_dir"
    return 1
}
zle -N __zsh_reload_theme
zle -F -w "$__zsh_theme_fd" __zsh_reload_theme
autoload -Uz add-zsh-hook
add-zsh-hook precmd __zsh_theme_prompt
add-zsh-hook zshexit __zsh_theme_cleanup
