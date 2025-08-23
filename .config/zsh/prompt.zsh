autoload -U colors
colors

setopt PROMPT_SUBST
PROMPT=$'\n%(?.%{$fg[green]%}$.%{$fg[red]%}$)%b '
