ssh-term() {
    bash -c "TERM=xterm ssh -t $1 $2"
}

ngtshead-term() {
    ssh-term ngtshead.astro "$*"
}

_node-loadavg() {
    echo watch -n 120 /home/sw/.bin/ngts/node-loadavg
}

_watchjobs() {
    echo /home/sw/.bin/watchjobs
}

_copywatcher() {
    echo /home/sw/.local/bin/tmux attach -t copy
}

node-loadavg() {
    ngtshead-term "$(_node-loadavg)"
}

watchjobs() {
    ngtshead-term "$(_watchjobs)"
}

copywatcher() {
    ngtshead-term "$(_copywatcher)"
}
