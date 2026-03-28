# create a new directory and cd into it
function mcd() {
    mkdir -p $1 && cd $1
}

function tcd() {
    cd "$(mktemp -d $TMPDIR/tempXXXX)"
    tnew
}

function op-signin() {
    if ! op account list 2>/dev/null | grep -q my.1password.com; then
        op account add --address my.1password.com --shorthand my
    fi
    eval "$(op signin --account my)"
}
