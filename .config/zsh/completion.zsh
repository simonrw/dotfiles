typeset -U fpath
fpath=(
    "$HOME/.config/zsh/func"
    /opt/homebrew/share/zsh/site-functions
    /usr/local/share/zsh/site-functions
    /nix/var/nix/profiles/default/share/zsh/site-functions
    $fpath
)

autoload -Uz compinit
compinit -i -d "${ZDOTDIR:-$HOME}/.zcompdump-${ZSH_VERSION}"

zstyle ':completion:*' menu select
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh"

if (( ${+commands[aws_completer]} )); then
    autoload -Uz bashcompinit
    bashcompinit
    complete -C "$commands[aws_completer]" aws
fi

if (( ${+commands[jj-hp]} )); then
    eval "$(COMPLETE=zsh jj-hp)"
    compdef _clap_dynamic_completer_jj_hooks jj-hp
fi
