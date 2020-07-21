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

function hr() {
    printf '%*s\n' "${COLUMNS:-$(tput cols)}" '' | tr ' ' -
}

ARROW_ICON="➜"
export PROMPT=$'\n%F{4}%~%F{7}\n%F{yellow}$(__suspended_count)%(?.%F{green}${ARROW_ICON}.%F{red}${ARROW_ICON})%F{reset} '
export RPROMPT=$'%F{yellow}%m%F{reset}'
# vim: ft=zsh
