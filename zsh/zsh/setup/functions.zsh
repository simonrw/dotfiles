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
    ssh -t ngtshead $@ exec zsh
}

function ng.astro() {
    ssh -t ngtshead.astro $@ exec zsh
}

function mutt() {
    (cd ${HOME}/Downloads && =mutt)
}

function init-python() {
    local readonly fname="$1"
    # Remove the filename from the arugment list
    shift
    cat >${fname} <<EOL
#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import division, print_function, absolute_import
import argparse
import logging

logging.basicConfig(
    level='INFO', format='[%(asctime)s] %(levelname)8s %(message)s')
logger = logging.getLogger(__name__)


def main(args):
    if args.verbose:
        logger.setLevel('DEBUG')
    logger.debug(args)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-v', '--verbose', action='store_true')
    main(parser.parse_args())
EOL
    chmod +x ${fname}
    \vim ${fname} "${@}"
}

function init-3() {
    echo "use conda 3" >> .envrc && direnv allow && init-python ./main.py +qa
}

function init-2() {
    echo "use conda 2" >> .envrc && direnv allow && init-python ./main.py +qa
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

# Stop opening emacs in a tmux session
function emacs() {
    if [[ ! -z ${TMUX} ]]; then
        echo "Running emacs from within tmux is insane. Use =emacs if you're sure" >&2
    else
        =emacs $*
    fi
}
