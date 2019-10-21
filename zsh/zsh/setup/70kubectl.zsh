if has_executable kubectl; then
    eval "$(kubectl completion zsh)"
fi

alias kdump="kubectl get all --all-namespaces"
