# vim: ft=zsh
case $OSTYPE in linux*)
    export PATH=${HOME}/.rbenv/bin:${PATH}
    if [ $(type rbenv >/dev/null 2>&1) ]; then
        eval "$(rbenv init - --no-rehash)"
    fi
;; esac
