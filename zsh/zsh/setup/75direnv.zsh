if ! has_executable direnv; then
    muted_print "direnv not installed"
else
    eval "$(direnv hook zsh)"
fi
