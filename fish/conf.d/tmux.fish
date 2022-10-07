function _not_inside_tmux
    test -z "$TMUX"
end

function _not_inside_neovim
    test -z "$NVIM"
end

function _not_inside_emacs
    test -z "$INSIDE_EMACS" && test -z "$EMACS"
end

function _not_inside_vscode_term
    test "$TERM_PROGRAM" != "vscode"
end

function _not_inside_zellij
    test -z "$ZELLIJ_SESSION_NAME"
end

function _not_inside_pycharm
    test -z "$PYCHARM"
end

function _inside_x_session
    switch (uname)
        case Linux
            pgrep Xorg 2>&1 >/dev/null
        case '*'
            return 0
    end
end

function ensure_tmux_is_running
    if _not_inside_tmux && _not_inside_neovim && _not_inside_emacs && _inside_x_session && _not_inside_vscode_term && _not_inside_zellij && _not_inside_pycharm
        tat
    end
end

ensure_tmux_is_running
