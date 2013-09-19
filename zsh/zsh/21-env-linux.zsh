# vim: ft=zsh
case $OSTYPE in linux*)
    export LD_LIBRARY_PATH=${HOME}/prefix/lib:${LD_LIBRARY_PATH}

    # Set up the module command
    function module() { eval `modulecmd zsh $*`; }

    local autoenv_file=${HOME}/.autoenv/activate.sh
    if [ -f $autoenv_file ]; then
        source $autoenv_file
    fi

    # If the server is at Leicester, source the intel compiler variables
    if [[ `dnsdomainname` == "star.le.ac.uk" ]]; then
        source /opt/intel/composerxe-2011.0.084/bin/compilervars.sh intel64 2>/dev/null
        source /usr/local/sge/default/common/settings.sh
    fi
;; esac
