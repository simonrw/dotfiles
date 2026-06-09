# Ignore duplicate history entries
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

# Speed up completion
zstyle ':completion:*' accept-exact '*(N)'

# Use a cache for completion
zstyle ':completion:*' use-cache on

autoload -Uz compinit
typeset -ga __ZSH_COMPINIT_HOOKS

__zsh_compinit() {
    emulate -L zsh

    (( ${__ZSH_COMPINIT_DONE:-0} )) && return 0
    unfunction compdef 2>/dev/null

    local zcompdump="${ZDOTDIR:-$HOME}/.zcompdump-${ZSH_VERSION}"
    if [[ ! -f "$zcompdump" || -n "$zcompdump"(#qN.mh+24) ]]; then
        compinit -u -d "$zcompdump" || return
    else
        compinit -C -u -d "$zcompdump" || return
    fi

    typeset -g __ZSH_COMPINIT_DONE=1

    if (( ${+commands[aws]} && ${+commands[aws_completer]} )); then
        autoload -Uz bashcompinit
        bashcompinit
        complete -C "$commands[aws_completer]" aws
    fi

    local hook
    for hook in "${__ZSH_COMPINIT_HOOKS[@]}"; do
        (( ${+functions[$hook]} )) && "$hook"
    done
}

__zsh_add_compinit_hook() {
    emulate -L zsh

    local hook="$1"
    __ZSH_COMPINIT_HOOKS+=("$hook")
    (( ${__ZSH_COMPINIT_DONE:-0} )) && (( ${+functions[$hook]} )) && "$hook"
}

compdef() {
    unfunction compdef 2>/dev/null
    __zsh_compinit || return
    compdef "$@"
}

if [[ -o interactive ]]; then
    if [[ -z "${__ZSH_COMPLETION_WIDGET:-}" ]]; then
        typeset __zsh_completion_tab_binding
        __zsh_completion_tab_binding=$(bindkey '^I' 2>/dev/null)
        [[ "$__zsh_completion_tab_binding" =~ 'undefined-key' ]] || __ZSH_COMPLETION_WIDGET=$__zsh_completion_tab_binding[(s: :w)2]
        [[ "$__ZSH_COMPLETION_WIDGET" == __zsh_lazy_completion_widget ]] && __ZSH_COMPLETION_WIDGET=expand-or-complete
        unset __zsh_completion_tab_binding
    fi

    __zsh_lazy_completion_widget() {
        __zsh_compinit || return
        zle "${__ZSH_COMPLETION_WIDGET:-expand-or-complete}"
    }

    zle -N __zsh_lazy_completion_widget
    bindkey '^I' __zsh_lazy_completion_widget
fi

if [ -f /Applications/Ghostty.app/Contents/Resources/zsh/site-functions/_ghostty ]; then
    # TODO
    # eval /Applications/Ghostty.app/Contents/Resources/zsh/site-functions/_ghostty
fi
