if ! has_executable direnv; then
    muted_print "direnv not installed"
fi

eval "$(direnv hook zsh)"
