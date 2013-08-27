# vim: ft=zsh
case $OSTYPE in darwin*)
    export PATH=${PATH}:/usr/texbin:/usr/local/share/npm/bin
    if [[ -f /usr/local/opt/autoenv/activate.sh ]]; then
        source /usr/local/opt/autoenv/activate.sh
    fi
;; esac
