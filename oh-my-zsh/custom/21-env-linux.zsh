# vim: ft=zsh
case $OSTYPE in linux*)
    export LD_LIBRARY_PATH=${HOME}/prefix/lib:${LD_LIBRARY_PATH}
;; esac
