# Disable Ctrl-s freezing the terminal
stty stop undef 2>/dev/null

# Disable greeting
set -g fish_greeting

# Tool integrations (order matters: fzf before atuin so atuin overrides Ctrl-R)
mise activate fish | source
fzf --fish | source
atuin hex init fish | source

# Theme
if test "$__IS_DARK_THEME" = 1
    fish_config theme choose --color-theme=dark base16-default
else
    fish_config theme choose --color-theme=light default
end

# Per-host config
set -l this_hostname (hostname -s)
if test -f ~/.config/fish/conf.d/per-host/$this_hostname.fish
    source ~/.config/fish/conf.d/per-host/$this_hostname.fish
end

# Local overrides
if test -f ~/.config/fish/local.fish
    source ~/.config/fish/local.fish
end
