# repo-managed zsh plugins
test -f ~/.config/zsh/defer.zsh && source ~/.config/zsh/defer.zsh
test -f ~/.config/zsh/lightweight-abbr.zsh && source ~/.config/zsh/lightweight-abbr.zsh

_source_first_deferred() {
    local file
    for file in "$@"; do
        if [[ -r "$file" ]]; then
            zsh-defer source "$file"
            return
        fi
    done
}

# zsh autosuggestions
_source_first_deferred \
    /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh \
    /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# zsh syntax highlighting must be loaded after other plugins that add ZLE widgets.
_source_first_deferred \
    /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh \
    /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

unfunction _source_first_deferred
