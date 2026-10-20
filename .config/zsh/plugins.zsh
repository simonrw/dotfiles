source "$HOME/.config/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(zsh-abbr-expand-accept-line shell-agent-execute)
ZSH_AUTOSUGGEST_IGNORE_WIDGETS+=(__zsh_reload_theme)

# Keep this last so highlighting sees changes from all other widgets.
source "$HOME/.config/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
