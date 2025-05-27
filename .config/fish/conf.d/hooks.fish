function refresh-tmux
    if test -n "$TMUX"
        export (tmux show-environment | grep '^SSH_AUTH_SOCK')
    end
end

function my_signal_handler --on-event fish_prompt
    # This function is called before every command is executed.
    # You can add any pre-execution logic here.
    refresh-tmux
end
