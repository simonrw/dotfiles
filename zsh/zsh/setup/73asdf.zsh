if [[ -e ${HOME}/.asdf ]]; then
    # Do not activate asdf if inside a poetry shell
    if [[ -z "${POETRY_ACTIVE}" ]]; then
        . ${HOME}/.asdf/asdf.sh
        . ${HOME}/.asdf/completions/asdf.bash
    fi
fi
