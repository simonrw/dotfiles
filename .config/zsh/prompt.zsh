autoload -U colors
colors

setopt PROMPT_SUBST
PROMPT=$'\n\n%(?.%{$fg[green]%}$.%{$fg[red]%}$)%b '
