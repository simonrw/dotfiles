# vim: ft=zsh
setopt interactive_comments
bindkey "^R" history-incremental-search-backward # Put the reverse history back
setopt rmstarsilent
setopt append_history

# Only unique history entries in the reverse history search HIST_FIND_NO_DUPS=1
setopt hist_ignore_all_dups
setopt hist_ignore_dups


# Ignore duplicate history entries
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes


HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
