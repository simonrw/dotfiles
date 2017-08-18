function __is_git_dir() {
    if [[ -d ".git" ]]; then
echo "[g]"
    else
echo ""
    fi
}

function __get_load() {
    uptime | awk '{print $(NF-2),$(NF-1),$(NF)}' | tr -d ' '
}

function __get_nr_jobs() {
    jobs | wc -l | sed 's/ //g'
}

function __suspended_count() {
    if [[ -n $(jobs -s) ]]; then
        print '= '
    fi
}

local green="%{$fg[green]%}"
local red="%{$fg[red]%}"
local yellow="%{$fg[yellow]%}"
local blue="%{$fg[cyan]%}"
local reset="%{$reset_color%}"
local grey="%{$fg[black]%}"

function hr() {
    printf '%*s\n' "${COLUMNS:-$(tput cols)}" '' | tr ' ' -
}

PROMPT=$'\n$grey%~\n$yellow$(__suspended_count)%(?.$green\\$.$red\\$)$reset '
# vim: ft=zsh
