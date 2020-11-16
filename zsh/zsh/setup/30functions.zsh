# Function to attach to a session. If the session is not specified then
#just run `tmux attach`, otherwise add a -t flag
function _tmux_attach() {
    if [ $1 ]; then
        tmux attach -t $1
    else
        tmux attach
    fi
}

# Function to read man pages
function vman() {
    vim \
        -c "source \$VIMRUNTIME/ftplugin/man.vim" \
        -c "Man $*" \
        -c "set number relativenumber readonly|only"
}

# Function to set terminal title
function title() {
    local readonly text="$1"
    printf "\033]0;${text}\007"
}

# From GRB
function mcd() { mkdir -p $1 && cd $1 }

# Function to make a new tmux session at a variable location
function tnew() {
    if [[ $# -gt 0 ]]; then
        if [[ $# -gt 1 ]]; then
            return
        fi

        DIRNAME="$1"
        if [[ ! -d ${DIRNAME} ]]; then
            mkdir ${DIRNAME}
        fi

    else
        DIRNAME=$(pwd)
    fi

    cd $DIRNAME
    TMUXNAME="$(basename $(dirname $PWD))/$(basename $PWD)"
    tmux new-session -d -s ${TMUXNAME} -n ''

    tmux attach -t ${TMUXNAME}
}

function init-python() {
    local readonly fname="$1"
    # Remove the filename from the arugment list
    shift
    cat >${fname} <<EOL
#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import division, print_function, absolute_import
import logging

logging.basicConfig(
    level=logging.INFO, format='%(asctime)s : %(message)s')
logger = logging.getLogger(__name__)


def main(args):
    if args.verbose:
        logger.setLevel(logging.DEBUG)
    logger.debug('%s', args)

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-v', '--verbose', action='store_true')
    main(parser.parse_args())
EOL
    chmod +x ${fname}
    \vim ${fname} "${@}"
}

# Function to start jupyter notebook
function jn() {
    jupyter notebook --no-browser $@
}

# Get server SSL certificate fingerprint in MD5, SHA1 and SHA256.
# Note that OpenSSL doesn't support IPv6 at time of writing (2015-01-13).
function serversslcertfp () {
    SSSLCFFN=$(openssl s_client -showcerts -connect $1 < /dev/null)
    # To see all validity information
    echo "$SSSLCFFN"
    # For getting the fingerprints
    echo "$SSSLCFFN" | openssl x509 -md5 -fingerprint -noout
    echo "$SSSLCFFN" | openssl x509 -sha1 -fingerprint -noout
    echo "$SSSLCFFN" | openssl x509 -sha256 -fingerprint -noout
    echo "$SSSLCFFN" | openssl x509 -sha512 -fingerprint -noout
    unset SSSLCFFN
}

# Function to add timestamps to all output
function add_timestamps() {
    awk '{print strftime("[%Y/%m/%d %H:%M:%S]: ")$0}'
}

# Function to lock the screen
function lockscreen() {
    case $OSTYPE in
        darwin*)
            /System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend
            ;;
    esac
}

function build-movie() {
    local dirname="$1"
    local output="$2"

    local fulloutpath="$(readlink -f ${output})"

    echo "Rendering movie out of pngs in ${dirname} to ${fulloutpath}"

    (cd ${dirname} && mencoder 'mf://*.png' -mf w=800:h=600:fps=30:type=png -ovc x264 -x264encopts crf=18:nofast_pskip:nodct_decimate:nocabac:global_header:threads=12 -of lavf -lavfopts format=mp4 -o "${fulloutpath}")
}

# Expose the current directory using a python webserver
# - handles importing http.server or SimpleHTTPServer
function webshare() {
    if has_executable python3; then
        python3 -m http.server $*
    else
        python2 -m SimpleHTTPServer $*
    fi
}

# Speed up git completion
__git_files () {
    _wanted files expl 'local files' _files
}

# Functions to load mutt. They change the current directory to the download
# directory so that any attachments are automatically saved there
function mutt() {
    (cd ~/Downloads
    if has_executable neomutt; then
        =neomutt;
    else
        =mutt
    fi)
}
alias email=mutt

function lb() {
    LOGBOOK_DIR=${HOME}/logbook
    if [ ! -d ${LOGBOOK_DIR} ]; then
        mkdir -p ${LOGBOOK_DIR}
    fi

    vim ${LOGBOOK_DIR}/$(date '+%Y-%m-%d').md
}

# Function to create a quick git commit
function commit() {
    if [[ $# == 0 ]]; then
        git commit
    else
        git commit -m "$*"
    fi
}

alias c=commit
alias com=commit

# Function to add to my did.txt file
function did() {
    =vim +'normal Go' +'r!date' ~/did.txt
}

function task() {
    if [ -f ${PWD}/.taskrc ]; then
        =task rc:${PWD}/.taskrc $*
    else
        =task $*
    fi
}

_not_inside_tmux() { [[ -z "$TMUX" ]] }

# Prevent nested tmux problem
function ssh() {
    if _not_inside_tmux; then
        =ssh $*
    else
        # Check hostname is not in blacklist
        for name in "astoria"; do
            if $(echo "$*" | grep -q "astoria"); then
                echo "Cannot ssh to this host inside the current tmux session. The host is also configured to use tmux and so would cause a nested tmux issue." >&2
                return
            fi
        done

        =ssh $*
    fi
}

# Function to source the current virtual environment if there is one
function se() {
    if has_executable direnv; then
        grep -q 'layout python-venv' .envrc 2>/dev/null || {
            echo layout python-venv >> .envrc
            direnv allow .
        }
    else
        if [[ -f ./venv/bin/activate ]]; then
            # Have to check that the shell is not currently in a virtual environment
            test -z $VIRTUAL_ENV || {
                echo "Current shell is in a virtual environment ($VIRTUAL_ENV). Deactivating" >&2
                deactivate
            }
            source ./venv/bin/activate
        else
            # Create the virtual environment
            echo "Virtual environment does not exist, creating" >&2
            if has_executable python3; then
                python3 -m venv ./venv
            else
                # Hope python is python3!
                python -m venv ./venv
            fi
            echo "Installing latest version of pip"
            ./venv/bin/pip install -U pip
            se
        fi
    fi
}

# Function to get the hierarchical process tree upwards (usually ending at PID 1)
function pparents() {
    if [[ $# -ne 1 ]]; then
        echo "Usage: pparents <pid>" >&2
        return 1
    fi

    pstree -s -p $1
}

# Function to grep for specific process lines
function psgrep() {
    if [[ $# -ne 1 ]]; then
        echo "Usage: psgrep <name>" >&2
        return 1
    fi

    ps aux | grep $1
}

function find_pi() {
    if [[ $# != 1 ]]; then
        echo "usage: find_pi <interface>" >&2
        return 1
    fi

    local interface=$1

    echo "Usage may require sudo password"
    sudo arp-scan --interface=$interface --localnet | grep b8:27:eb | awk '{print $1}'
}

# if no arguments are given then open with the file browser, otherwise pass arguments in
function vim() {
    if [[ $# -eq 0 ]]; then
        if has_executable nvim; then
            nvim +FzfGitFiles
        else
            =vim +FzfGitFiles
        fi
    else
        if has_executable nvim; then
            nvim $*
        else
            =vim $*
        fi
    fi
}

# function to edit a note from the command line
function note() {
    if [[ $# -eq 0 ]]; then
        # XXX we may need to allow the user to configure the notes directory
        NOTESDIR=$HOME/notes/
        STUB="$(find $NOTESDIR -type f -print0 | xargs -0 basename | sort | fzf -0 --preview 'bat -l markdown --color=always $HOME/notes/{}')"
        vim +edit "note:$STUB"
    else
        vim +edit "note:$*"
    fi
}

function wakeastoria() {
    wakeonlan -i 192.168.0.255 -p 7 40:B0:76:DE:79:B3 >/dev/null
    readonly ip_address="$(nslookup astoria | grep Address | grep 192 | awk '{print $2}')"
    test -z ${ip_address} && {
        ping -o ${ip_address} >/dev/null
    } || {
        ping -o 192.168.0.10 >/dev/null
    }
}

function pushdotfiles() {
    (cd ${HOME}/dotfiles && __pushdotfiles)
}

function __pushdotfiles() {
    git diff --quiet HEAD || {
        echo "dotfiles dir not clean"
        return
    }

    git push
}
