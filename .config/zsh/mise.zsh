if (( ${+commands[mise]} )); then
    typeset -gi __ZSH_MISE_LOADED=0

    __zsh_mise_load() {
        (( __ZSH_MISE_LOADED )) && return 0

        local activation
        activation="$(command mise activate zsh)" || return
        eval "$activation" || return
        __ZSH_MISE_LOADED=1
    }

    __zsh_mise_preexec() {
        __zsh_mise_load && add-zsh-hook -d preexec __zsh_mise_preexec 2>/dev/null
    }

    mise() {
        __zsh_mise_load || return
        mise "$@"
    }

    autoload -Uz add-zsh-hook
    add-zsh-hook preexec __zsh_mise_preexec
fi
