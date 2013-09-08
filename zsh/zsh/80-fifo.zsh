local fifo_name='.fifo'
ensure_fifo() {
    if [ ! -p $fifo_name ]; then
        mkfifo $fifo_name
    fi
}

listen() {
    echo "Listening for commands"
    ensure_fifo && while true; do sh -c "$(cat $fifo_name)"; done
}
