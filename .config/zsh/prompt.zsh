autoload -U colors
colors

setopt PROMPT_SUBST
PROMPT=$'\n${ZMX_SESSION:+%{$fg[blue]%}[$ZMX_SESSION] %{$reset_color%}}%(?.%{$fg[green]%}$.%{$fg[red]%}$)%b '
