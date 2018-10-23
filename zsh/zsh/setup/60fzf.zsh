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

function fpass() {
  if ! lpass status -q; then
    lpass login $EMAIL
  fi

  if ! lpass status -q; then
    exit
  fi

  lpass ls | fzf-tmux | =grep -oE '\d+' | xargs lpass show -c --password
}

# Overload the default history widget which reverses the order of the entries
function fzf-history-widget () {
    local selected num
    setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
    selected=($(fc -l 1 |
        FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS --query=${(qqq)LBUFFER} +m" $(__fzfcmd)))
    local ret=$?
    if [ -n "$selected" ]
    then
        num=$selected[1]
        if [ -n "$num" ]
        then
            zle vi-fetch-history -n $num
        fi
    fi
    zle reset-prompt
    return $ret
}
