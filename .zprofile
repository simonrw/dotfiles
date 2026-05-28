if [ -f /opt/homebrew/bin/mise ]; then
    export PATH=$PATH:/opt/homebrew/bin
fi

# set up mise early on
export PATH="${HOME}/.local/share/mise/shims:${PATH}"

# ensure bob is on the path
export PATH=${PATH}:${HOME}/.local/share/bob/nvim-bin
