autoload -Uz compinit
if [ "$(date +'%j')" != "$(stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)" ]; then
    compinit
else
    compinit -C
fi

if [ -f /Applications/Ghostty.app/Contents/Resources/zsh/site-functions/_ghostty ]; then
    # TODO
    # eval /Applications/Ghostty.app/Contents/Resources/zsh/site-functions/_ghostty
fi


# Ignore duplicate history entries
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

# Speed up completion
zstyle ':completion:*' accept-exact '*(N)'

# Use a cache for completion
zstyle ':completion:*' use-cache on
