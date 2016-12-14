[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

fs() {
    local session
    session=$(tmux list-sessions -F "#{session_name}" | \
        fzf-tmux --query="$1" --select-1 --exit-0)

    if [[ ! -z ${session} ]]; then
        if [ -z ${TMUX} ]; then
            tmux attach -t "$session"
        else
            tmux switch-client -t "$session"
        fi
    fi
}

gco() {
    local tags branches target
    tags=$(
    git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return
    branches=$(
    git branch --all | grep -v HEAD             |
    sed "s/.* //"    |
    sort -u          | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
    target=$(
    (echo "$tags"; echo "$branches") |
    fzf-tmux -- --no-hscroll --ansi +m -d "\t" -n 2) || return
    git checkout $(echo "$target" | awk '{print $2}')
}

# fcs - get git commit sha
# example usage: git rebase -i `fcs`
gsha() {
  local commits commit
  commits=$(git log --all --color=always --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e --ansi --reverse) &&
  echo -n $(echo "$commit" | sed "s/ .*//")
}

# Bind ctrl-f to find using default fzf command, in addition to ctrl-t
bindkey '^F' fzf-file-widget
