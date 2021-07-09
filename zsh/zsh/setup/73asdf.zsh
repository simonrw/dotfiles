if test -f ${HOME}/.asdf/asdf.sh; then
. ${HOME}/.asdf/asdf.sh
else
    muted_print "asdf not installed"
fi

# append completions to fpath
fpath=(${ASDF_DIR}/completions $fpath)

# Optimize startup by only loading completion once per day
# Taken from https://gist.github.com/ctechols/ca1035271ad134841284#gistcomment-3109177
() {
  if [[ $# -gt 0 ]]; then
    compinit
  else
    compinit -C
  fi
} ${ZDOTDIR:-$HOME}/.zcompdump(N.mh+24)

