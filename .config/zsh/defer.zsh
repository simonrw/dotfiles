typeset -gi _zsh_defer_available=0
if [[ -o interactive ]] && zmodload zsh/sched 2>/dev/null; then
    _zsh_defer_available=1
fi

zsh-defer() {
    emulate -L zsh

    if (( ! _zsh_defer_available )); then
        "$@"
        return
    fi

    sched +0 "${(j: :)${(q)@}}"
}
