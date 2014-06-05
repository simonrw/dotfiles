function __is_git_dir() {
    if [[ -d ".git" ]]; then
echo "[g]"
    else
echo ""
    fi
}

function get_load() {
    uptime | awk '{print $(NF-2),$(NF-1),$(NF)}' | tr -d ' '
}

function get_nr_jobs() {
    jobs | wc -l | sed 's/ //g'
}

local green="%{$fg[green]%}"
local red="%{$fg[red]%}"
local yellow="%{$fg[yellow]%}"
local blue="%{$fg[cyan]%}"
local reset="%{$reset_color%}"

PROMPT='
%(?.$green.$red)>$reset '
RPROMPT=''

# vim: ft=zsh
