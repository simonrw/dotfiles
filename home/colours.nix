{ config, lib, ... }:
let
  themes = {
    gruvbox = rec {
      tmux-colour = normal.yellow;
      fish-theme = (throw "not set");

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
      fish-theme = "Tomorrow Night Bright";

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
    srw = rec {
      # Default colors
      primary = {
        background = "#07151a";
        foreground = "#ffffff";
      };

      tmux-colour = normal.blue;
      fish-theme = "fish default";

      # Normal colors
      normal = {
        black = "#616261";
        red = "#fd8272";
        green = "#b4fa73";
        yellow = "#fefcc3";
        blue = "#a5d5fe";
        magenta = "#fd8ffd";
        cyan = "#d0d1fe";
        white = "#f1f0f2";
      };

      # Bright colors
      bright = {
        black = "#8d8e8d";
        red = "#fec4bd";
        green = "#d6fcb9";
        yellow = "#fefdd5";
        blue = "#c1e3fe";
        magenta = "#fdb1fe";
        cyan = "#e5e6fe";
        white = "#fefffe";
      };

      # Cursor colors
      cursor = {
        text = "#000000";
        cursor = "#ffffff";
      };

      # Vi mode cursor colors
      vi_mode_cursor = {
        text = "#000000";
        cursor = "#ffffff";
      };

      # Selection colors
      selection = {
        text = "#eaeaea";
        background = "#404040";
      };

      # Dim colors
      dim = {
        black = "#131415";
        red = "#864343";
        green = "#777c44";
        yellow = "#9e824c";
        blue = "#556a7d";
        magenta = "#75617b";
        cyan = "#5b7d78";
        white = "#828482";
      };
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
    srw = ''
      set background=dark
      colorscheme srw256
      hi Normal guibg=none
      let g:linenr_background = 'none'
      execute 'highlight TelescopeNormal guibg=' . g:linenr_background
      execute 'highlight LineNr guibg=' . g:linenr_background
      execute 'highlight SignColumn guibg=' . g:linenr_background
      execute 'highlight GitGutterAdd guibg=' . g:linenr_background
      execute 'highlight GitGutterDelete guibg=' . g:linenr_background
      execute 'highlight GitGutterChange guibg=' . g:linenr_background
      highlight TabLine guibg=none
      highlight TabLineSel guibg=none
      highlight TabLineFill guibg=none
      execute 'highlight DiagnosticSignError ctermfg=1 guifg=Red guibg=' . g:linenr_background
      execute 'highlight DiagnosticSignHint ctermfg=7 guifg=LightGrey guibg=' . g:linenr_background
      execute 'highlight DiagnosticSignInfo ctermfg=4 guifg=LightBlue guibg=' . g:linenr_background
      execute 'highlight DiagnosticSignWarn ctermfg=3 guifg=Orange guibg=' . g:linenr_background
      highlight DiagnosticUnderlineHint guifg=Grey guisp=Grey
    '';
  };

  current-theme = themes.${config.me.theme};
  tmux-primary-colour = current-theme.tmux-colour;
  tmux-background-colour = current-theme.primary.background;
  fish-theme = current-theme.fish-theme;
in
with lib;

{
  options = {
    me.theme = mkOption {
      type = types.enum [
        "github"
        "srw"
        "gruvbox"
      ];
    };
  };
  config = {
    programs.fish.interactiveShellInit = ''
      # configure colour theme
      fish_config theme choose "${fish-theme}"
    '';
    programs.alacritty.settings.colors = themes.${config.me.theme};
    programs.neovim.extraConfig = neovim-theme-blocks.${config.me.theme};
    programs.tmux.extraConfig = ''
      fg_colour="${tmux-primary-colour}"
      bg_colour="${tmux-background-colour}"

      set -g mode-style "fg=#d5e1ed,bg=#456075"

      set -g message-style "fg=#dddddd,bg=$bg_colour"
      set -g message-command-style "fg=#dddddd,bg=$bg_colour"

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
      setw -g window-status-format " #[fg=#777777]#I #W:#{pane_current_command}#F "
      setw -g window-status-current-format " #[fg=#dddddd]#I #W:#{pane_current_command}#F "
    '';
    programs.kitty.settings = {
      background = current-theme.primary.background;
      foreground = current-theme.primary.foreground;

      cursor = current-theme.cursor.cursor;
      cursor_text_color = current-theme.cursor.text;

      selection_foreground = current-theme.selection.text;
      selection_background = current-theme.selection.background;

      color0 = current-theme.normal.black;
      color1 = current-theme.normal.red;
      color2 = current-theme.normal.green;
      color3 = current-theme.normal.yellow;
      color4 = current-theme.normal.blue;
      color5 = current-theme.normal.magenta;
      color6 = current-theme.normal.cyan;
      color7 = current-theme.normal.white;

      color8 = current-theme.bright.black;
      color9 = current-theme.bright.red;
      color10 = current-theme.bright.green;
      color11 = current-theme.bright.yellow;
      color12 = current-theme.bright.blue;
      color13 = current-theme.bright.magenta;
      color14 = current-theme.bright.cyan;
      color15 = current-theme.bright.white;
    };
  };
}
