ssh-term() {
    urxvt -e bash -c "TERM=xterm ssh -t $1 $2"
}

ngtshead-term() {
    ssh-term ngtshead.astro "$*"
}

node-loadavg() {
    ngtshead-term watch -n 120 /home/sw/.bin/ngts/node-loadavg
}

watchjobs() {
    ngtshead-term /home/sw/.bin/watchjobs
}

copywatcher() {
    ngtshead-term /home/sw/.local/bin/tmux attach -t copy
}
