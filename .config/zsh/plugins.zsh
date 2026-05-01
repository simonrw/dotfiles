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
# Use an explicit grey rather than the plugin default (fg=8), because some terminal
# themes map ANSI bright black to the normal foreground colour.
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=244'
_source_first_deferred \
    /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh \
    /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# zsh syntax highlighting must be loaded after other plugins that add ZLE widgets.
_source_first_deferred \
    /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh \
    /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

unfunction _source_first_deferred
