typeset -gi _lazy_compinit_loaded=0
typeset -ga _lazy_compdefs

# Queue completion registrations until the first completion request.
compdef() {
    _lazy_compdefs+=("${(j: :)${(q)@}}")
}

_setup_aws_completion() {
    (( _lazy_compinit_loaded )) || return
    (( _aws_completion_loaded )) && return

    zmodload -F zsh/parameter p:commands 2>/dev/null
    (( $+commands[aws] && $+commands[aws_completer] )) || return

    autoload -Uz bashcompinit
    bashcompinit
    complete -C "$commands[aws_completer]" aws
    _aws_completion_loaded=1
}

_lazy_compinit() {
    (( _lazy_compinit_loaded )) && return

    autoload -Uz compinit

    local zcompdump="${ZDOTDIR:-$HOME}/.zcompdump-${ZSH_VERSION}"
    if [[ ! -f "$zcompdump" || -n "$zcompdump"(#qN.mh+168) ]]; then
        compinit -u -d "$zcompdump"
    else
        compinit -C -u -d "$zcompdump"
    fi

    _lazy_compinit_loaded=1

    local spec
    for spec in $_lazy_compdefs; do
        eval "compdef $spec"
    done
    _lazy_compdefs=()

    _setup_aws_completion
}

_lazy_expand_or_complete() {
    _lazy_compinit
    zle expand-or-complete
}

# Ignore duplicate history entries
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

# Speed up completion
zstyle ':completion:*' accept-exact '*(N)'

# Use a cache for completion
zstyle ':completion:*' use-cache on

if [ -f /Applications/Ghostty.app/Contents/Resources/zsh/site-functions/_ghostty ]; then
    # TODO
    # eval /Applications/Ghostty.app/Contents/Resources/zsh/site-functions/_ghostty
fi

typeset -gi _aws_completion_seen_first_prompt=0
typeset -gi _aws_completion_loaded=0

# Check AWS completion after the first prompt; actual setup waits for compinit.
_load_aws_completion() {
    if (( ! _aws_completion_seen_first_prompt )); then
        _aws_completion_seen_first_prompt=1
        return
    fi

    add-zsh-hook -d precmd _load_aws_completion
    _setup_aws_completion
}

if [[ -o interactive ]]; then
    autoload -Uz add-zsh-hook
    add-zsh-hook precmd _load_aws_completion
    zle -N _lazy_expand_or_complete
    bindkey '^I' _lazy_expand_or_complete
fi
