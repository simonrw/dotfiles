function _tmux_attach --description="Attach to a tmux session"
    if test (count $argv) -gt 0
        tmux attach -t $argv[1]
    else
        tmux attach
    end
end
