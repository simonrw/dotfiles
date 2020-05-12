test -f ${HOME}/.asdf/asdf.sh || {
    muted_print "asdf not installed"
    exit 1
}

. ${HOME}/.asdf/asdf.sh
