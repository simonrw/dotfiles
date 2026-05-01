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

# set up aws completion
if command -v aws >/dev/null 2>&1; then
    autoload bashcompinit && bashcompinit
    complete -C $(which aws_completer) aws
fi
