# Disable Ctrl-s freezing the terminal
stty stop undef 2>/dev/null

# Disable greeting
set -g fish_greeting

# Tool integrations
if type -q mise
    mise activate fish | source
end

if type -q fzf
    fzf --fish | source
end

# Theme
if test "$__IS_DARK_THEME" = 1
    fish_config theme choose --color-theme=dark catppuccin-mocha
else
    fish_config theme choose --color-theme=light catppuccin-mocha
end

# shell agent
if test -f {$HOME}/.config/fish/plugins/shell-agent/fish/shell-agent.fish; and type -q codex
    source {$HOME}/.config/fish/plugins/shell-agent/fish/shell-agent.fish
    shell_agent_enable
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
