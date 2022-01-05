# Set up skim
export SKIM_DEFAULT_OPTIONS="--tiebreak begin --color light,matched_bg:-1 --ansi --no-mouse --tabstop 4 --inline-info"
export SKIM_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,venv,node_modules}/*" 2> /dev/null'


fs() {
    local session
    session=$(tmux list-sessions -F "#{session_name}" | \
        sk-tmux --query="$1" --select-1 --exit-0)

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
    sk-tmux -- --no-hscroll --ansi -d "\t" -n 2) || return
    git checkout $(echo "$target" | awk '{print $2}')
}

# fcs - get git commit sha
# example usage: git rebase -i `fcs`
gsha() {
  local commits commit
  commits=$(git log --all --color=always --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | sk --tac -e --ansi --reverse) &&
  echo -n $(echo "$commit" | sed "s/ .*//")
}

__skimcmd() {
  [ -n "$TMUX_PANE" ] && { [ "${SKIM_TMUX:-0}" != 0 ] || [ -n "$SKIM_TMUX_OPTS" ]; } &&
    echo "sk-tmux ${SKIM_TMUX_OPTS:--d${SKIM_TMUX_HEIGHT:-40%}} -- " || echo "sk"
}

# Overload the default history widget which reverses the order of the entries
skim-history-widget() {
  local selected num
  setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null
  selected=( $(fc -rl 1 | perl -ne 'print if !$seen{(/^\s*[0-9]+\**\s+(.*)/, $1)}++' |
    SKIM_DEFAULT_OPTIONS="--height ${SKIM_TMUX_HEIGHT:-40%} --color light,matched_bg:-1 --ansi --no-mouse --tabstop 4 --inline-info -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $SKIM_CTRL_R_OPTS --query=${(qqq)LBUFFER} --no-multi" $(__skimcmd)) )
  local ret=$?
  if [ -n "$selected" ]; then
    num=$selected[1]
    if [ -n "$num" ]; then
      zle vi-fetch-history -n $num
    fi
  fi
  zle reset-prompt
  return $ret
}
zle     -N   skim-history-widget
bindkey '^R' skim-history-widget
