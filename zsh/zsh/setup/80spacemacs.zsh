function __install_spacemacs() {
    echo "Installing spacemacs to ~/.emacs.d"
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
}

function __spacemacs_installed() {
    \grep -qi spacemacs ~/.emacs.d/README.md 2>/dev/null
}

if [[ ! -d ~/.emacs.d ]]; then
    __install_spacemacs
else
    if ! __spacemacs_installed; then
        __install_spacemacs 2>/dev/null || echo "Cannot install spacemacs. Perhaps ~/.emacs.d already exists and is not a spacemacs installation?"
    fi
fi

