set -x FZF_DEFAULT_OPTIONS "--color dark,matched_bg:-1 --tiebreak begin --ansi --no-mouse --tabstop 4 --inline-info"
set -x FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow -g "!{.git,venv,node_modules}/*" 2> /dev/null'

function fs
    tmux-session-history
end
