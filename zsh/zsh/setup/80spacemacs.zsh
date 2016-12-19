function __install_spacemacs() {
    echo "Installing spacemacs to ~/.emacs.d"
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
}

function __spacemacs_installed() {
    \grep -qi spacemacs ~/.emacs.d/README.md 2>/dev/null
}

if [[ ! -d ~/.emacs.d ]]; then
    __install_spacemacs
fi

if ! __spacemacs_installed; then
    __install_spacemacs
fi

