if ! has_executable direnv; then
    muted_print "direnv not installed"
fi

alias de='direnv edit .'
alias da='direnv allow'

eval "$(direnv hook zsh)"
