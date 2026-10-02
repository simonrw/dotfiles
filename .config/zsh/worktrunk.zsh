if (( ${+commands[wt]} )) || [[ -n "${WORKTRUNK_BIN:-}" ]]; then
    __zsh_worktrunk_load() {
        local init
        init=$(command "${WORKTRUNK_BIN:-wt}" config shell init zsh) || return
        eval "$init"
    }

    wt() {
        __zsh_worktrunk_load || return
        wt "$@"
    }

    _wt_lazy_complete() {
        __zsh_worktrunk_load || return
        _wt_lazy_complete "$@"
    }
    compdef _wt_lazy_complete wt
fi
