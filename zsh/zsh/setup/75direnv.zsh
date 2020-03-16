if ! has_executable direnv; then
    printf "\u001b[30;1mdirenv not installed\u001b[0m\n"
else
    eval "$(direnv hook zsh)"
fi
