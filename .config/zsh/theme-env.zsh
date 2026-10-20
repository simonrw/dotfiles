# Shared by shell startup and in-place theme refreshes.
if is-dark-theme; then
    export __IS_DARK_THEME=1
    export FZF_DEFAULT_OPTS='--tiebreak begin --ansi --no-mouse --tabstop 4 --inline-info --color dark'
    export BAT_CONFIG_PATH="$HOME/.config/bat/dark/config"
    export GLAMOUR_STYLE=dark
    export K9S_SKIN=catppuccin-macchiato
else
    export __IS_DARK_THEME=0
    export FZF_DEFAULT_OPTS='--tiebreak begin --ansi --no-mouse --tabstop 4 --inline-info --color light'
    export BAT_CONFIG_PATH="$HOME/.config/bat/light/config"
    export GLAMOUR_STYLE=light
    export K9S_SKIN=catppuccin-latte
fi
