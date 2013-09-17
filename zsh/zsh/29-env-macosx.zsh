# vim: ft=zsh
case $OSTYPE in darwin*)
    export PATH=${PATH}:/usr/texbin:/usr/local/share/npm/bin:/usr/local/sbin
    local autoenv_file=/usr/local/opt/autoenv/activate.sh
    if [ -f $autoenv_file ]; then
        source $autoenv_file
    fi
;; esac
