# create a new directory and cd into it
function mcd() {
    mkdir -p $1 && cd $1
}

function tcd() {
    cd "$(mktemp -d $TMPDIR/tempXXXX)"
    tnew
}

function session-history() {
    if test -z "${TMUX:-}"; then
        builtin cd $(command zoxide query --list | fzf)
    else
        builtin cd $(command zoxide query --list | fzf)
    fi
}

function project() {
    z $(listprojects --list | fzf)
}
