if command -v wt >/dev/null 2>&1 || [[ -n "${WORKTRUNK_BIN:-}" ]]; then
    typeset -gi __ZSH_WORKTRUNK_LOADED=0

    __zsh_worktrunk_load() {
        (( __ZSH_WORKTRUNK_LOADED )) && return 0

        local init
        init="$(command wt config shell init zsh)" || return
        eval "$init" || return
        __ZSH_WORKTRUNK_LOADED=1
    }

    wt() {
        __zsh_worktrunk_load || return
        wt "$@"
    }

    _wt_lazy_complete() {
        __zsh_worktrunk_load || return
        _wt_lazy_complete "$@"
    }

    __zsh_worktrunk_compinit() {
        if (( ${+functions[compdef]} )); then
            compdef _wt_lazy_complete wt
            zstyle ':completion:*:wt:*' list-max 1
            zstyle ':completion:*:*:wt:*' list-grouped false
        fi
    }

    if (( ${+functions[__zsh_add_compinit_hook]} )); then
        __zsh_add_compinit_hook __zsh_worktrunk_compinit
    else
        __zsh_worktrunk_compinit
    fi
fi
