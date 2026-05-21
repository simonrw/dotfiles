autoload -U colors
colors

setopt PROMPT_SUBST
PROMPT=$'%(?.%{$fg[green]%}$.%{$fg[red]%}$)%b '
