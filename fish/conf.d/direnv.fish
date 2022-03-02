if not type -q direnv
    echo "direnv not installed"
end

alias de 'direnv edit .'
alias da 'direnv allow'

eval (direnv hook fish)
