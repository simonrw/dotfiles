_not_inside_tmux() { [[ -z "$TMUX" ]] }

_not_inside_neovim() { [[ -z "$NVIM_LISTEN_ADDRESS" ]] }

_not_inside_emacs() { [[ -z "$INSIDE_EMACS" && -z "$EMACS" ]] }

_not_inside_vscode_term() { [[ "$TERM_PROGRAM" != "vscode" ]] }

# We only want tmux to run when were inside a X session i.e. not at a virtual
# console. On macos we have to pretend that an Xsession is running so hard code
# the result.
_inside_x_session() {
    case $OSTYPE in
        darwin*)
            # Hard code this to always return an x session
            return 0
            ;;
        linux*)
            pgrep Xorg 2>&1 >/dev/null
            ;;
    esac
}

ensure_tmux_is_running() {
    if _not_inside_tmux && _not_inside_neovim && _not_inside_emacs && _inside_x_session && _not_inside_vscode_term; then
        tat
    fi
}

# helper functions

# remove old tmux sessions from more than a week ago
function tmux-prune() {
    now="$(date +%s)"
    cutoff="$(python3 -c "print(${now} - 604800)")"

    tmux ls -F '#S #{session_activity}' | while read info; do
        name="$(echo $info | cut -f 1 -d ' ')"
        last_activity="$(echo $info | cut -f 2 -d ' ')"

        if [ $last_activity -lt $cutoff ]; then
            echo "Pruning tmux session $name"
            tmux kill-session -t $name
        fi
    done
}

tmux-prune
ensure_tmux_is_running
