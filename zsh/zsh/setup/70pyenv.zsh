export PATH=${HOME}/.pyenv/bin:${PATH}

if ! has_executable "pyenv"; then
    # Install pyenv using pyenv-installer
    echo "Installing pyenv"
    curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
fi
    # Shim to set up pyenv
eval "$(command pyenv init - --no-rehash)"
