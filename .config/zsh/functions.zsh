# create a new directory and cd into it
function mcd() {
    mkdir -p $1 && cd $1
}

function tcd() {
    cd "$(mktemp -d)"
    pwd
}
