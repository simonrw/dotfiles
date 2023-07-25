{ config, lib, ... }:
let
  themes = rec {
    github-light = rec {
      primary = {
        background = "#ffffff";
        foreground = "#24292f";
      };

      cursor = {
        text = normal.black;
        cursor = normal.white;
      };

      selection = {
        text = normal.white;
        background = normal.blue;
      };

      tmux-colour = normal.blue;
      tmux-active-pane-colour = normal.blue;
      tmux-pane-colour = normal.black;
      fish-theme = "Tomorrow";

      # Normal colors
      normal = {
        black = "#24292e";
        red = "#d73a49";
        green = "#28a745";
        yellow = "#dbab09";
        blue = "#0366d6";
        magenta = "#5a32a3";
        cyan = "#0598bc";
        white = "#6a737d";
      };

      # Bright colors
      bright = {
        black = "#959da5";
        red = "#cb2431";
        green = "#22863a";
        yellow = "#b08800";
        blue = "#005cc5";
        magenta = "#5a32a3";
        cyan = "#3192aa";
        white = "#d1d5da";
      };

      indexed_colors = [
        { index = 16; color = "#d18616"; }
        { index = 17; color = "#cb2431"; }
      ];
    };
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

      tmux-colour = "#a5d6ff";
      tmux-active-pane-colour = tmux-colour;
      tmux-pane-colour = normal.black;

      fish-theme = "fish default";
      cursor = {
        text = normal.black;
        cursor = normal.white;
      };

      selection = {
        text = normal.white;
        background = normal.blue;
      };

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
      tmux-active-pane-colour = normal.blue;
      tmux-pane-colour = normal.white;
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
    monochrome = srw;
    monokai-pro = rec {
      cursor = {
        text = normal.black;
        cursor = normal.white;
      };

      selection = {
        text = normal.white;
        background = normal.blue;
      };

      primary = {
        background = "#2D2A2E";
        foreground = "#fff1f3";
      };

      # Normal colors
      normal = {
        black = "#2c2525";
        red = "#fd6883";
        green = "#adda78";
        yellow = "#f9cc6c";
        blue = "#a8a9eb";
        magenta = "#f38d70";
        cyan = "#85dacc";
        white = "#fff1f3";
      };

      # Bright colors
      bright = {
        black = "#72696a";
        red = "#fd6883";
        green = "#adda78";
        yellow = "#f9cc6c";
        blue = "#a8a9eb";
        magenta = "#f38d70";
        cyan = "#85dacc";
        white = "#fff1f3";
      };

      tmux-colour = normal.yellow;
      tmux-active-pane-colour = normal.white;
      tmux-pane-colour = normal.yellow;
      fish-theme = "fish default";
    };
  };
  neovim-theme-blocks = {
    github-light = ''
      set background=light
      colorscheme github_light
      highlight DiagnosticError guifg=Red
      highlight DiagnosticHint guifg=Orange
      highlight DiagnosticWarn guifg=Orange
      highlight DiagnosticInfo guifg=LightBlue
      highlight DiagnosticFloatingError guifg=Red
      highlight DiagnosticFloatingHint guifg=Orange
      highlight DiagnosticFloatingInfo guifg=LightBlue
      highlight DiagnosticFloatingWarn guifg=Orange
      highlight DiagnosticVirtualTextError guifg=Red
      highlight DiagnosticVirtualTextHint guifg=Orange
      highlight DiagnosticVirtualTextInfo guifg=LightBlue
      highlight DiagnosticVirtualTextWarn guifg=Orange
      highlight Comment guifg=#e69340   " brighten comments
      highlight TreesitterContext guibg=#eeeeee
    '';
    github = ''
      set background=dark
      colorscheme github_dark
      " overrides
      highlight Comment guifg=#e69340   " brighten comments
      highlight TSComment guifg=#e69340   " brighten comments
      highlight Normal guibg=none
      highlight NormalNC guibg=none
    '';
    gruvbox = ''
      set background=dark
      colorscheme base16-gruvbox-dark-hard
      highlight Comment guifg=#e69340   " brighten comments
      highlight TSComment guifg=#e69340   " brighten comments
    '';
    monochrome = ''
      set background=dark
      colorscheme fogbell
      hi Normal guibg=none
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
    monokai-pro = ''
      set background=dark
      colorscheme monokai-pro
      highlight Comment guifg=#e69340   " brighten comments
      highlight TSComment guifg=#e69340   " brighten comments
      highlight NormalFloat guibg=#343136
      highlight TabLine gui=none guibg=none guifg=${themes.monokai-pro.normal.yellow}
      highlight TabLineSel gui=none guibg=none guifg=${themes.monokai-pro.normal.white}
      highlight TabLineFill gui=none guibg=none
    '';
  };

  helix-theme = {
    github-light = "github_light";
  }.${config.me.theme} or "monokai-pro-custom";

  current-theme = themes.${config.me.theme};
  tmux-primary-colour = current-theme.tmux-colour;
  tmux-background-colour = current-theme.primary.background;
  tmux-active-pane-text-colour = current-theme.tmux-active-pane-colour;
  tmux-pane-text-colour = current-theme.tmux-pane-colour;
  fish-theme = current-theme.fish-theme;

  delta-theme = {
    github-light = "GitHub";
  }.${config.me.theme} or "Monokai Extended";
  delta-diff-so-fancy = config.me.theme != "github-light";

  bat-theme = {
    github-light = "GitHub";
  }.${config.me.theme} or "Monokai Extended";

  vscode-theme = { }.${config.me.theme} or "Monokai Pro";

in
with lib;

{
  options = {
    me.theme = mkOption {
      type = types.enum [
        # dark themes
        "github"
        "srw"
        "gruvbox"
        "monokai-pro"
        "monochrome"
        # light themes
        "github-light"
      ];
    };

    me.vscode-theme = mkOption {
      type = types.nullOr types.str;
      description = "Custom vscode theme if different";
      default = null;
    };
  };
  config = {
    programs.bat.config.theme = bat-theme;
    programs.git.delta.options.diff-so-fancy = delta-diff-so-fancy;
    programs.helix.settings.theme = helix-theme;
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

      set -g message-style "fg=${tmux-active-pane-text-colour},bg=$bg_colour"
      set -g message-command-style "fg=${tmux-active-pane-text-colour},bg=$bg_colour"

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
      setw -g window-status-format " #[fg=${tmux-pane-text-colour}]#I #W:#{pane_current_command}#F "
      setw -g window-status-current-format " #[fg=${tmux-active-pane-text-colour}]#I #W:#{pane_current_command}#F "
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

    programs.git.delta.options.syntax-theme = delta-theme;
    programs.vscode.userSettings = {
      "workbench.colorTheme" = config.me.vscode-theme or ({ }.${config.me.theme} or "Monokai Pro");
    };
  };
}
