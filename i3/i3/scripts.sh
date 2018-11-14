ssh-term() {
    bash -c "TERM=xterm ssh -t $1 $2"
}
