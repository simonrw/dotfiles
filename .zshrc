source ~/.config/zsh/env.zsh
source ~/.config/zsh/aliases.zsh
source ~/.config/zsh/fzf.zsh
if [ -f ~/.config/zsh/per-host/$(hostname).zsh ]; then source ~/.config/zsh/per-host/$(hostname).zsh; fi
source ~/.config/zsh/functions.zsh
source ~/.config/zsh/options.zsh
source ~/.config/zsh/completion.zsh
