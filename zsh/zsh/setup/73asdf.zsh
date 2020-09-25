if test -f ${HOME}/.asdf/asdf.sh; then
. ${HOME}/.asdf/asdf.sh
else
    muted_print "asdf not installed"
fi

# append completions to fpath
fpath=(${ASDF_DIR}/completions $fpath)
# initialise completions with ZSH's compinit
autoload -Uz compinit
compinit
