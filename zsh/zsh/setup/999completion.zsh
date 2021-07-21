fpath=(${ASDF_DIR}/completions $HOME/.zsh/completion $fpath)

# Optimize startup by only loading completion once per day
# Taken from https://gist.github.com/ctechols/ca1035271ad134841284#gistcomment-3109177
() {
  if [[ $# -gt 0 ]]; then
    compinit
  else
    compinit -C
  fi
} ${ZDOTDIR:-$HOME}/.zcompdump(N.mh+24)

autoload -U bashcompinit
bashcompinit

complete -o nospace -o default -o bashdefault -F _python_argcomplete pipx
