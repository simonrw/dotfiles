# Set up zoxide
#
# https://github.com/ajeetdsouza/zoxide

if has_executable zoxide; then
    source ${HOME}/dotfiles/external/zoxide/zoxide.plugin.zsh
else
    printf "\u001b[30;1mZoxide not installed\u001b[0m\n"
fi
