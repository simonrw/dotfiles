function __suspended_count() {
    if [[ -n $(jobs -s) ]]; then
        print '= '
    fi
}


__prompt_icon="$"

export PROMPT=$'\n%F{yellow}$(__suspended_count)%(?.%F{green}${__prompt_icon}.%F{red}${__prompt_icon})%F{reset} '
export RPROMPT=$''
# vim: ft=zsh
