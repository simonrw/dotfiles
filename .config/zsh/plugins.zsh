# repo-managed zsh plugins
test -f ~/.config/zsh/lightweight-abbr.zsh && source ~/.config/zsh/lightweight-abbr.zsh

# zsh syntax highlighting
test -f /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh && source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
test -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh && source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# zsh autosuggestions
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=244'
test -f /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh && source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
test -f /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh && source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
