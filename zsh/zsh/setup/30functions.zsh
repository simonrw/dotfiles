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
    TMUXNAME=$(basename `pwd`)
    tns ${TMUXNAME}

}

function fancy-ctrl-z () {
    if [[ $#BUFFER -eq 0 ]]; then
        BUFFER="fg"
        zle accept-line
    else
        zleush-input
        zle clear-screen
    fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

# SSH into the ngts ops machine, allowing for custom ssh arguments
function ng() {
    ssh -t ngtshead $@ exec /home/sw/.local/bin/zsh
}

function ng.astro() {
    ssh -t ngtshead.astro $@ exec /home/sw/.local/bin/zsh
}

function ng.pi() {
    ssh -t ngtshead.pi $@ exec /home/sw/.local/bin/zsh
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
    level='INFO', format='%(asctime)s : %(message)s')
logger = logging.getLogger(__name__)


def main(args):
    if args.verbose:
        logger.setLevel('DEBUG')
    logger.debug(args)

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

# Function to source the virtual environment in the current directory
function sourceenv() {
    local readonly _SRW_OLD_PS1="${PS1}"
    if [[ -f venv/bin/conda ]]; then
        # Conda environment
        # Ensure the pyenv alias has loaded
        pyenv >/dev/null 2>&1
        source $(pyenv which activate) ${PWD}/venv
    else
        # Normal python environment
        source ${PWD}/venv/bin/activate
    fi
    export PS1="${_SRW_OLD_PS1}"
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
