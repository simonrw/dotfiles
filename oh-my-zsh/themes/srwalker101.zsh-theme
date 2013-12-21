function __is_git_dir() {
    if [[ -d ".git" ]]; then
echo "[g]"
    else
echo ""
    fi
}

local green="%{$fg_bold[green]%}"
local red="%{$fg_bold[red]%}"
local yellow="%{$fg_bold[yellow]%}"
local reset="%{$reset_color%}"

PROMPT="%(?.$green.$red%? )\$$reset "
RPROMPT='%2~$(__is_git_dir)@$yellow%m$reset'

# vim: ft=zsh
