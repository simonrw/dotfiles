if [ -f /opt/homebrew/bin/mise ]; then
    export PATH=$PATH:/opt/homebrew/bin
fi

# ensure bob is on the path
export PATH=${PATH}:${HOME}/.local/share/bob/nvim-bin
