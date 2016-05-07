[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

fs() {
    local session
    session=$(tmux list-sessions -F "#{session_name}" | \
        fzf-tmux --query="$1" --select-1 --exit-0)

    if [ -z ${TMUX} ]; then
        tmux attach -t "$session"
    else
        tmux switch-client -t "$session"
    fi
}
