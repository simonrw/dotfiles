{ config, lib, ... }:
let
  themes = {
    gruvbox = rec {
      tmux-colour = normal.yellow;

      primary = {
        background = "#1d2021";
        foreground = "#d5c4a1";
      };

      # Colors the cursor will use if `custom_cursor_colors` is true
      cursor = {
        text = "#1d2021";
        cursor = "#d5c4a1";
      };

      # Normal colors
      normal = {
        black = "#1d2021";
        red = "#fb4934";
        green = "#b8bb26";
        yellow = "#fabd2f";
        blue = "#83a598";
        magenta = "#d3869b";
        cyan = "#8ec07c";
        white = "#d5c4a1";

      };
      # Bright colors
      bright = {
        black = "#665c54";
        red = "#fe8019";
        green = "#3c3836";
        yellow = "#504945";
        blue = "#bdae93";
        magenta = "#ebdbb2";
        cyan = "#d65d0e";
        white = "#fbf1c7";
      };
    };
    github = rec {
      # github Alacritty Colors
      # Default colors
      primary = {
        background = "#24292e";
        foreground = "#edf0f2";
      };

      tmux-colour = normal.blue;

      # Normal colors
      normal = {
        black = "#586069";
        red = "#ea4a5a";
        green = "#34d058";
        yellow = "#ffea7f";
        blue = "#5f9fe8";
        magenta = "#b392f0";
        cyan = "#39c5cf";
        white = "#d1d5da";
      };

      # Bright colors
      bright = {
        black = "#959da5";
        red = "#f97583";
        green = "#85e89d";
        yellow = "#ffea7f";
        blue = "#79b8ff";
        magenta = "#b392f0";
        cyan = "#56d4dd";
        white = "#fafbfc";
      };

      indexed_colors = [
        { index = 16; color = "#d18616"; }
        { index = 17; color = "#f97583"; }
      ];
    };
  };
  neovim-theme-blocks = {
    github = ''
      set background=dark
      colorscheme github_dark
      " overrides
      highlight Comment guifg=#e69340   " brighten comments
      highlight TSComment guifg=#e69340   " brighten comments
    '';
    gruvbox = ''
      set background=dark
      colorscheme base16-gruvbox-dark-hard
      highlight Comment guifg=#e69340   " brighten comments
      highlight TSComment guifg=#e69340   " brighten comments
    '';
  };

  tmux-primary-colour = themes.${config.me.theme}.tmux-colour;
  tmux-background-colour = themes.${config.me.theme}.primary.background;
in
with lib;

{
  options = {
    me.theme = mkOption {
      type = types.enum [
        "github"
        "gruvbox"
      ];
    };
  };
  config = {
    programs.alacritty.settings.colors = themes.${config.me.theme};
    programs.neovim.extraConfig = neovim-theme-blocks.${config.me.theme};
    programs.tmux.extraConfig = ''
      fg_colour="${tmux-primary-colour}"
      bg_colour="${tmux-background-colour}"

      set -g mode-style "fg=#d5e1ed,bg=#456075"

      set -g message-style "fg=#c9d1d9,bg=$bg_colour"
      set -g message-command-style "fg=#c9d1d9,bg=$bg_colour"

      set -g pane-border-style "fg=#444c56"
      set -g pane-active-border-style "fg=$fg_colour"

      set -g status "on"
      set -g status-justify "left"

      set -g status-style "fg=$fg_colour,bg=$bg_colour"
      set -g status-bg "$bg_colour"

      set -g status-left-length "100"
      set -g status-right-length "100"

      set -g status-left-style NONE
      set -g status-right-style NONE

      set -g status-left "#[fg=$fg_colour,bold][#S] "
      set -g status-right "#[fg=$fg_colour,bold]#h"

      setw -g window-status-activity-style "underscore,fg=#d1d5da,bg=$bg_colour"
      setw -g window-status-separator ""
      setw -g window-status-format "#[fg=#777777]#I #W:#{pane_current_command}#F"
      setw -g window-status-current-format "#[fg=#dddddd]#I #W:#{pane_current_command}#F"
    '';
  };
}
