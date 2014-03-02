function __is_git_dir() {
    if [[ -d ".git" ]]; then
echo "[g]"
    else
echo ""
    fi
}

function get_load() {
    uptime  | cut -d ' ' -f 12-14
}

local green="%{$fg_bold[green]%}"
local red="%{$fg_bold[red]%}"
local yellow="%{$fg_bold[yellow]%}"
local reset="%{$reset_color%}"

PROMPT="
%(?.$green.$red)\$$reset "
RPROMPT='%2~$(__is_git_dir)|${red}[$(get_load)]$yellow%n$reset@$yellow%m${reset}'

# vim: ft=zsh
