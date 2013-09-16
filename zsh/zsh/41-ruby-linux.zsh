# vim: ft=zsh
case $OSTYPE in linux*)
    if [ -d ${HOME}/.rbenv ]; then
        export PATH=${HOME}/.rbenv/bin:${PATH}
        eval "$(rbenv init - --no-rehash)"
    fi
;; esac
