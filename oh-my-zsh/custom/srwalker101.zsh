export PATH=/usr/local/bin:${PATH}
fpath=($HOME/.zsh/func $fpath)
typeset -U fpath

autoload -U compinit
compinit


# Emacs keybindings
bindkey -e

# Set up editor in command line
autoload edit-command-line
zle -N edit-command-line
bindkey '^Xe' edit-command-line

autoload -U colors
colors

setopt interactivecomments
setopt rmstarsilent
setopt prompt_subst
setopt inc_append_history
setopt share_history
unsetopt auto_pushd

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

# Function to attach to a session. If the session is not specified then
#just run `tmux attach`, otherwise add a -t flag
function _tmux_attach() {
    if [ $1 ]; then
        tmux attach -t $1
    else
        tmux attach
    fi
}

# From GRB
function mcd() { mkdir -p $1 && cd $1 }
function cdf() { cd *$1*/ }

function ignore_untracked_files() {
    =git st |
    grep '??' |
    cut -d ' ' -f 2- >> .gitignore
}

# Alias some tmux commands
alias ta=_tmux_attach
alias tns="tmux new-session -s"
alias tls="tmux ls"

export GOPATH=${HOME}/Development/gocode
export PATH=${HOME}/.cabal/bin:${HOME}/prefix/bin:${HOME}/.rbenv/bin:${PATH}:${HOME}/.cask/bin:${GOPATH}/bin
# Ensure my .bin directory is first
export PATH=${HOME}/.bin:${PATH}
export EDITOR=vim_nox
export VISUAL=${EDITOR}
export BIBINPUTS=${HOME}/work/central-bibliography:${BIBINPUTS}
export TERM=xterm-256color

# Taken from grb's zshrc

# By default, zsh considers many characters part of a word (e.g., _ and -).
# Narrow that down to allow easier skipping through words via M-f and M-b.
export WORDCHARS='*?[]~&;!$%^<>'

alias -g py='=python'
alias -g ipy=ipython
alias python='echo "Use py..."'
alias pydoc='=python -m pydoc'
alias vim="echo 'Use v...'"
alias vi=vim_nox
alias ls='ls -F'
alias l=ls
alias g='git'
alias gst='=git st'
alias gcm='=git commit -m'
alias gse='=git sync && exit'
alias sc="v +Scratch"
alias es='exec $SHELL'

# Update vim plugins
alias vbi='=vim +BundleInstall +qa'
alias vbu='=vim +BundleUpdate'

# Ruby aliases
alias bcb='bundle check; bundle install --binstubs .bundle/bin'
alias be='bundle exec'

# Function for finding my pi
function __active_interface() {
    local ifconfig_path=/sbin/ifconfig
    for interface in en0 en1; do
        if ${ifconfig_path} ${interface} | xargs | grep -q 192 2>&1 >/dev/null; then
            echo $interface
            return
        fi
    done
}

function find_pi() {
    echo "Usage may require sudo password"
    sudo arp-scan --interface=`__active_interface` --localnet | grep b8:27:eb | awk '{print $1}'
}

case $OSTYPE in
    linux*)
        export LD_LIBRARY_PATH=${HOME}/prefix/lib:${LD_LIBRARY_PATH}

        # Set up the module command
        function module() { eval `modulecmd zsh $*`; }

        # If the server is at Leicester, source the intel compiler variables
        if [[ `dnsdomainname` == "star.le.ac.uk" ]]; then
            source /opt/intel/composerxe-2011.0.084/bin/compilervars.sh intel64 2>/dev/null
            source /usr/local/sge/default/common/settings.sh
        fi

        VIRTUALENV_DIR=${HOME}/PythonEnv
        VIRTUALENV_SOURCE_FILE=${VIRTUALENV_DIR}/bin/activate
        if [[ -f ${VIRTUALENV_SOURCE_FILE} ]]; then
            # Set up the python environment
            # Set the environment variable for only this env to disable the prompt
            VIRTUAL_ENV_DISABLE_PROMPT=1 source ${VIRTUALENV_SOURCE_FILE}
            # export PYTHONPATH=${VIRTUALENV_DIR}/lib/python2.7/site-packages:$PYTHONPATH
        fi

        alias lsc='ls --color=auto'

        ;;
    darwin*)
        export PATH=${PATH}:/usr/texbin:/usr/local/sbin:/sbin:/usr/sbin
        alias gvim=mvim
        alias gview=mview
        alias -g awk=gawk
        alias lsc='ls -G'
        ;;
esac

if which rbenv > /dev/null; then eval "$(rbenv init --no-rehash - zsh)"; fi

# Most recent history commands
function most_used_history() {
    history 1 |
    awk '{a[$2]++}END{for (i in a){print a[i] " " i}}' |
    sort -rn |
    head -n 20
}


# Source the custom zshrc.local file in the system
if [[ -f ${HOME}/.zshrc.local ]]; then
    source ${HOME}/.zshrc.local
fi

