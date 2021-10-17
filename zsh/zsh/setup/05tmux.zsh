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

# run a prune in the background, but only for interactive shells
ensure_tmux_is_running
