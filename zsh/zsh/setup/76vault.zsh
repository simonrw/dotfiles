if has_executable vault; then
    autoload -U +X bashcompinit && bashcompinit
    complete -o nospace -C $(which vault) vault
fi
