if [[ "$__ZSH_PROFILE_STARTUP" == true ]]; then
    zmodload zsh/zprof
fi

source ~/.config/zsh/functions.zsh
source ~/.config/zsh/env.zsh
source ~/.config/zsh/mise.zsh

if [[ -o interactive ]]; then
    source ~/.config/zsh/options.zsh
    source ~/.config/zsh/aliases.zsh
    source ~/.config/zsh/completion.zsh
    source ~/.config/zsh/fzf.zsh
    source ~/.config/zsh/prompt.zsh
    source ~/.config/zsh/theme.zsh
    source ~/.config/zsh/lightweight-abbr.zsh
    source ~/.config/zsh/worktrunk.zsh
    (( ${+commands[atuin]} )) && eval "$(atuin init zsh)"
    if (( ${+commands[codex]} )); then
        source ~/.config/zsh/shell-agent.zsh
        shell_agent_enable
    fi
fi

this_hostname=$(hostname -s)
[[ -f ~/.config/zsh/per-host/${this_hostname}.zsh ]] && source ~/.config/zsh/per-host/${this_hostname}.zsh
unset this_hostname
[[ -f ~/.config/zsh/local.zsh ]] && source ~/.config/zsh/local.zsh

# Syntax highlighting must load after every widget and key binding.
[[ -o interactive ]] && source ~/.config/zsh/plugins.zsh

if [[ "$__ZSH_PROFILE_STARTUP" == true ]]; then
    zprof
fi
