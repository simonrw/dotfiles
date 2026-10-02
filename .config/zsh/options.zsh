# Disable Ctrl-s freezing the terminal.
[[ -t 0 ]] && stty stop undef 2>/dev/null

setopt interactivecomments rmstarsilent auto_cd
unsetopt bang_hist
setopt inc_append_history share_history hist_ignore_all_dups hist_ignore_dups
unsetopt auto_pushd beep

WORDCHARS='*?[]~&;!$%^<>'
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=$HISTSIZE

bindkey -e
bindkey '^?' backward-delete-char
bindkey '^R' history-incremental-search-backward
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '^[[3~' delete-char
bindkey '^[[1;3C' forward-word
bindkey '^[[1;3D' backward-word

autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^Xe' edit-command-line
