# Inline expansions stay separate from aliases, as in Fish.
typeset -gA ZSH_ABBREVIATIONS ZSH_COMMAND_ABBREVIATIONS

abbr() {
    emulate -L zsh
    local command_name=
    if [[ "$1" == --command ]]; then
        command_name=$2
        shift 2
    fi

    local definition key expansion
    for definition in "$@"; do
        key=${definition%%=*}
        expansion=${definition#*=}
        if [[ -n "$command_name" ]]; then
            ZSH_COMMAND_ABBREVIATIONS[$command_name:$key]=$expansion
        else
            ZSH_ABBREVIATIONS[$key]=$expansion
        fi
    done
}

_zsh_abbr_expand_lbuffer() {
    emulate -L zsh
    local -a tokens
    tokens=(${(z)LBUFFER})
    (( ${#tokens} )) || return 0

    # Quoted words and partially typed tokens must not expand.
    local key=$tokens[-1] command_name= token expansion=
    [[ "$LBUFFER" == *"$key" ]] || return 0
    for token in "${(@)tokens[1,-2]}"; do
        case "$token" in
            ';'|'|'|'||'|'&'|'&&'|$'\n'|'(') command_name= ;;
            *)
                if [[ -z "$command_name" && "$token" != [A-Za-z_]*=* ]]; then
                    command_name=$token
                fi ;;
        esac
    done

    if [[ -z "$command_name" ]]; then
        expansion=${ZSH_ABBREVIATIONS[$key]}
    else
        expansion=${ZSH_COMMAND_ABBREVIATIONS[$command_name:$key]}
    fi
    [[ -n "$expansion" ]] || return 0
    LBUFFER="${LBUFFER[1,$(( ${#LBUFFER} - ${#key} ))]}$expansion"
}

_zsh_abbr_expand_space() {
    _zsh_abbr_expand_lbuffer
    zle .self-insert
}

_zsh_abbr_expand_accept_line() {
    _zsh_abbr_expand_lbuffer
    unset POSTDISPLAY
    zle .accept-line
}

source "$HOME/.config/zsh/abbreviations"

zle -N zsh-abbr-expand-space _zsh_abbr_expand_space
zle -N zsh-abbr-expand-accept-line _zsh_abbr_expand_accept_line
bindkey -M emacs ' ' zsh-abbr-expand-space
bindkey -M emacs '^M' zsh-abbr-expand-accept-line
bindkey -M emacs '^J' zsh-abbr-expand-accept-line
