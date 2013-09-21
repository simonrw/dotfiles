# vim: ft=zsh
setopt interactive_comments
bindkey "^R" history-incremental-search-backward # Put the reverse history back
setopt rmstarsilent
setopt append_history

# Only unique history entries in the reverse history search HIST_FIND_NO_DUPS=1
setopt hist_ignore_all_dups
setopt hist_ignore_dups


# Ignore duplicate history entries
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes


HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
## vim: ft=zsh
# Function to attach to a session. If the session is not specified then
#just run `tmux attach`, otherwise add a -t flag
function _tmux_attach() {
    if [ $1 ]; then
        tmux attach -t $1
    else
        tmux attach
    fi
}

# Alias some tmux commands
alias ta=_tmux_attach
alias tns="tmux new-session -s"
alias tls="tmux ls"

# vim: ft=zsh
export PATH=${HOME}/.bin:${HOME}/prefix/bin:$PATH
export EDITOR=vim_nox
export VISUAL=${EDITOR}
export BIBINPUTS=${HOME}/work/central-bibliography:${BIBINPUTS}
export TERM=screen-256color

# Taken from grb's zshrc

# By default, zsh considers many characters part of a word (e.g., _ and -).
# Narrow that down to allow easier skipping through words via M-f and M-b.
export WORDCHARS='*?[]~&;!$%^<>'
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
# vim: ft=zsh
case $OSTYPE in darwin*)
    export PATH=${PATH}:/usr/texbin:/usr/local/share/npm/bin:/usr/local/sbin
    local autoenv_file=/usr/local/opt/autoenv/activate.sh
    if [ -f $autoenv_file ]; then
        source $autoenv_file
    fi
;; esac
# vim: ft=zsh
alias pylab="ipython --pylab"
alias py='ipython --pylab'
alias pydoc='python -m pydoc'
alias vim=vim_nox
alias ls='ls -F'

# Git aliases
alias g=git

# Update vim plugins
alias vbi='vim +BundleInstall +qa'
alias vbu='vim +BundleUpdate'

# Ruby aliases
alias bcb='bundle check; bundle install --binstubs .bundle/bin'
# vim: ft=zsh
case $OSTYPE in darwin*)
    alias gvim=mvim
    alias gview=mview
    alias -g awk=gawk
;; esac

# vim: ft=zsh
case $OSTYPE in linux*)
    if [ -d ${HOME}/.rbenv ]; then
        export PATH=${HOME}/.rbenv/bin:${PATH}
        eval "$(rbenv init - --no-rehash)"
    fi
;; esac
# vim: ft=zsh
case $OSTYPE in darwin*)
    export RBENV_ROOT=/usr/local/var/rbenv
    if which rbenv > /dev/null; then eval "$(rbenv init - --no-rehash)"; fi
;; esac
# vim: ft=zsh
function gi() {
    # This function gets the gitignore results from gitignore.io and prints them
    # to screen
    #
    # Inputs: any extra arguments given on the command line are treated as
    # ignore classes for the website.
    #
    # E.g. > gi linux osx python
    #
    # will translate to http://gitignore.io/api/linux,osx,python
    URL=http://gitignore.io/api/
    ARGS=$(echo $* | sed 's/ /,/g')
    curl ${URL}${ARGS}
}

# From GRB
function mcd() { mkdir -p $1 && cd $1 }
function cdf() { cd *$1*/ }

# Source the custom zshrc.local file in the system
if [[ -f ${HOME}/.zshrc.local ]]; then
    source ${HOME}/.zshrc.local
fi
# vim: ft=zsh
case $OSTYPE in linux*)
    VIRTUALENV_DIR=${HOME}/PythonEnv
        VIRTUALENV_SOURCE_FILE=${VIRTUALENV_DIR}/bin/activate
        if [[ -f ${VIRTUALENV_SOURCE_FILE} ]]; then
                # Set up the python environment
                # Set the environment variable for only this env to disable the prompt
                VIRTUAL_ENV_DISABLE_PROMPT=1 source ${VIRTUALENV_SOURCE_FILE}
                # export PYTHONPATH=${VIRTUALENV_DIR}/lib/python2.7/site-packages:$PYTHONPATH
        fi
;; esac
local fifo_name='.fifo'
ensure_fifo() {
    if [ ! -p $fifo_name ]; then
        mkfifo $fifo_name
    fi
}

listen() {
    echo "Listening for commands"
    ensure_fifo && while true; do sh -c "$(cat $fifo_name)"; done
}
