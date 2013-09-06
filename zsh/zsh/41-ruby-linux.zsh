# vim: ft=zsh
case $OSTYPE in linux*)
    export PATH=${HOME}/.rbenv/bin:${PATH}
    eval "$(rbenv init - --no-rehash)"
;; esac
