__shell_agent_execute() {
    local raw=$BUFFER
    if [[ "$raw" =~ '^[[:space:]]*:' ]]; then
        local runner=${SHELL_AGENT_BIN:-shell-agent}
        if ! command -v -- "$runner" >/dev/null 2>&1; then
            zle -M "shell-agent: runner not found: $runner"
            return 127
        fi

        local prompt=${raw#*:}
        setopt localoptions extendedglob
        prompt=${prompt##[[:space:]]#}
        local prompt_file
        prompt_file=$(mktemp -t shell-agent-prompt.XXXXXX) || return
        printf '%s' "$prompt" > "$prompt_file" || return
        export __SHELL_AGENT_RUNNER=$runner
        export __SHELL_AGENT_PWD=$PWD
        export __SHELL_AGENT_PROMPT_FILE=$prompt_file
        BUFFER=__shell_agent_run
        unset POSTDISPLAY
        zle .accept-line
        return
    fi
    _zsh_abbr_expand_accept_line
}

__shell_agent_run() {
    printf '\033[1A\033[2K\r'
    "${__SHELL_AGENT_RUNNER:-shell-agent}" --pwd "${__SHELL_AGENT_PWD:-$PWD}" \
        --prompt-file "$__SHELL_AGENT_PROMPT_FILE"
    local result=$?
    unset __SHELL_AGENT_PROMPT_FILE
    return $result
}

shell_agent_enable() {
    zle -N shell-agent-execute __shell_agent_execute
    bindkey '^M' shell-agent-execute
    bindkey '^J' shell-agent-execute
}

shell_agent_disable() {
    bindkey '^M' zsh-abbr-expand-accept-line
    bindkey '^J' zsh-abbr-expand-accept-line
}
