autoload -U colors
colors

PROMPT=$'\n$fg[blue]%2~\n%(?.%{$fg[green]%}$.%{$fg[red]%}$)%b '
