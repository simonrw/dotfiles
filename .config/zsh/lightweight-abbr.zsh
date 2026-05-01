# Lightweight fish-style command-position abbreviations.
#
# This intentionally supports the existing ~/.config/zsh/abbreviations format:
#   abbr "g"="git"
#   abbr "wt switch"="wt switch --no-cd"
#
# It avoids zsh-abbr's persistence and job-queue setup on shell startup.

typeset -gA ZSH_ABBREVIATIONS

abbr() {
    emulate -L zsh

    local definition key expansion
    for definition in "$@"; do
        key=${definition%%=*}
        expansion=${definition#*=}
        [[ "$key" == "$definition" || -z "$key" ]] && continue
        ZSH_ABBREVIATIONS[$key]=$expansion
    done
}

_zsh_abbr_is_command_position() {
    emulate -L zsh
    setopt extendedglob

    local prefix="${1##*[;&|]}"
    prefix="${prefix##[[:space:]]##}"
    prefix="${prefix%%[[:space:]]##}"

    [[ -z "$prefix" || "$prefix" == sudo || "$prefix" == command || "$prefix" == builtin || "$prefix" == env ]]
}

_zsh_abbr_expand_lbuffer() {
    emulate -L zsh

    local key before best_key=
    for key in ${(k)ZSH_ABBREVIATIONS}; do
        [[ "$LBUFFER" == *"$key" ]] || continue

        before="${LBUFFER[1,$(( ${#LBUFFER} - ${#key} ))]}"
        _zsh_abbr_is_command_position "$before" || continue

        if (( ${#key} > ${#best_key} )); then
            best_key=$key
        fi
    done

    [[ -n "$best_key" ]] || return 0
    LBUFFER="${LBUFFER[1,$(( ${#LBUFFER} - ${#best_key} ))]}${ZSH_ABBREVIATIONS[$best_key]}"
}

_zsh_abbr_expand_space() {
    _zsh_abbr_expand_lbuffer
    LBUFFER+=" "
}

_zsh_abbr_expand_accept_line() {
    _zsh_abbr_expand_lbuffer
    zle .accept-line
}

if [[ -r "$HOME/.config/zsh/abbreviations" ]]; then
    source "$HOME/.config/zsh/abbreviations"
fi

if [[ -o interactive ]]; then
    zle -N _zsh_abbr_expand_space
    zle -N _zsh_abbr_expand_accept_line

    bindkey -M emacs ' ' _zsh_abbr_expand_space
    bindkey -M viins ' ' _zsh_abbr_expand_space
    bindkey -M emacs '^M' _zsh_abbr_expand_accept_line
    bindkey -M viins '^M' _zsh_abbr_expand_accept_line
fi
