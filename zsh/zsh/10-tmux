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

