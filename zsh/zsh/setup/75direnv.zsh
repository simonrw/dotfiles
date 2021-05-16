if ! has_executable direnv; then
    muted_print "direnv not installed"
else
    eval "$(asdf exec direnv hook zsh)"
fi

direnv() {
    asdf exec direnv "$@"
}
