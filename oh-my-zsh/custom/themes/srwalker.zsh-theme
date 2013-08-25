# print smiley for last command status
local process_status="%(?,%{$fg[green]%}✔%{$reset_color%},%{$fg[red]%}✗ %?%{$reset_color%})"

## Control promt
local blue="%{$fg[blue]%}"
local green="%{$fg[green]%}"
local red="%{$fg[red]%}"
local magenta="%{$fg[magenta]%}"
local reset="%{$reset_color%}"
local black="%{$fg[black]%}"
PROMPT=' 
${process_status} [${green}%m${reset}:${magenta}%2c${reset}|$(rbenv version-name)] 
> '
PROMPT2='. '
RPROMPT='${red}$(current_branch)${reset}'
