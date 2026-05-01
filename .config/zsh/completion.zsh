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

zcompdump="${ZDOTDIR:-$HOME}/.zcompdump-${ZSH_VERSION}"
if [[ ! -f "$zcompdump" || -n "$zcompdump"(#qN.mh+24) ]]; then
    compinit -u -d "$zcompdump"
else
    compinit -C -u -d "$zcompdump"
fi

if [ -f /Applications/Ghostty.app/Contents/Resources/zsh/site-functions/_ghostty ]; then
    # TODO
    # eval /Applications/Ghostty.app/Contents/Resources/zsh/site-functions/_ghostty
fi

typeset -gi _aws_completion_seen_first_prompt=0

# Set up AWS completion after the first prompt has been displayed.
_load_aws_completion() {
    if (( ! _aws_completion_seen_first_prompt )); then
        _aws_completion_seen_first_prompt=1
        return
    fi

    add-zsh-hook -d precmd _load_aws_completion

    zmodload -F zsh/parameter p:commands 2>/dev/null
    (( $+commands[aws] && $+commands[aws_completer] )) || return

    autoload -Uz bashcompinit
    bashcompinit
    complete -C "$commands[aws_completer]" aws
}

if [[ -o interactive ]]; then
    autoload -Uz add-zsh-hook
    add-zsh-hook precmd _load_aws_completion
fi
