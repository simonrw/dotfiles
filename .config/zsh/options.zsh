# Disable Ctrl-s freezing the terminal
stty stop undef

autoload add-zsh-hook

setopt interactivecomments
setopt rmstarsilent
setopt prompt_subst
setopt inc_append_history
setopt share_history
unsetopt auto_pushd
# Only unique history entries in the reverse history search HIST_FIND_NO_DUPS=1
setopt hist_ignore_all_dups
setopt hist_ignore_dups

# By default, zsh considers many characters part of a word (e.g., _ and -).
# Narrow that down to allow easier skipping through words via M-f and M-b.
export WORDCHARS='*?[]~&;!$%^<>'

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=$HISTSIZE


# Emacs keybindings
bindkey "^R" history-incremental-search-backward
bindkey -v '^?' backward-delete-char
bindkey -e

# Set up editor in command line
autoload edit-command-line
zle -N edit-command-line
bindkey '^Xe' edit-command-line
