function __suspended_count() {
    if [[ -n $(jobs -s) ]]; then
        print '= '
    fi
}


__prompt_icon="$"

export PROMPT=$'\n%F{yellow}$(__suspended_count)%(?.%F{10}${__prompt_icon}.%F{9}${__prompt_icon})%F{reset} '
export RPROMPT=$''
# vim: ft=zsh
