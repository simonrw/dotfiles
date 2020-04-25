bindkey -e

# View manpages in neovim
if has_executable nvim; then
    export MANPAGER="nvim -c 'set ft=man' -"
fi
