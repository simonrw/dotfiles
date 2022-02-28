set -x SKIM_DEFAULT_OPTIONS "--color dark,matched_bg:-1 --tiebreak begin --ansi --no-mouse --tabstop 4 --inline-info"
set -x SKIM_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow -g "!{.git,venv,node_modules}/*" 2> /dev/null'

function fs
    set session (tmux list-sessions -F "#{session_name}" | \
        sk-tmux --query="$1" --select-1 --exit-0)
    if test -n "$session"
        if test -z "$TMUX"
            tmux attach -t "$session"
        else
            tmux switch-client -t "$session"
        end
    end
end
