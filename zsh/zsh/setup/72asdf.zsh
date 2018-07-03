if [[ ! -d ${HOME}/.asdf ]]; then
    if has_executable "git"; then
        echo "Fetching asdf"
        git clone https://github.com/asdf-vm/asdf.git ${HOME}/.asdf
    else
        echo "Cannot install asdf without git :(" >&2
    fi
fi

. ${HOME}/.asdf/asdf.sh
. ${HOME}/.asdf/completions/asdf.bash
