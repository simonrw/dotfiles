if test -f ${HOME}/.asdf/asdf.sh; then
    . ${HOME}/.asdf/asdf.sh
elif test -f $(brew --prefix asdf)/libexec/asdf.sh; then
    . $(brew --prefix asdf)/libexec/asdf.sh
else
    muted_print "asdf not installed"
fi
