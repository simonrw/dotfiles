# vim: ft=zsh
case $OSTYPE in darwin*)
    export RBENV_ROOT=/usr/local/var/rbenv
    if which rbenv > /dev/null; then eval "$(rbenv init - --no-rehash)"; fi
;; esac
