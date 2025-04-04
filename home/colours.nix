{
  pkgs,
  config,
  lib,
  inputs,
  ...
}: let
  cfg = config.me;

  dark-themes = [
    "dracula"
    "github"
    "gruvbox"
    "monochrome"
    "monokai-pro"
    "nord"
    "one-dark"
    "srw"
    "catppuccin-frappe"
    "catppuccin-mocha"
    "catppuccin-macchiato"
    "poimandres"
  ];
  light-themes = [
    "catppuccin-latte"
    "github-light"
    "solarized-light"
    "papercolor"
  ];

  # custom vim plugins for colour schemes
  github-nvim-theme = pkgs.vimUtils.buildVimPlugin {
    pname = "github-nvim-theme";
    version = "1.0.1";
    src = pkgs.fetchFromGitHub {
      owner = "projekt0n";
      repo = "github-nvim-theme";
      rev = "d3199bc5074a80b123b6cd5b6797076b1727576a";
      hash = "sha256-4gyqucnN+dLxy5joPiKgJ6Ja5dpzfw0ADmUqQZQ9xg4=";
    };
  };

  themes = rec {
    one-dark = rec {
      fish-theme = "fish_default";

      cursor = {
        text = primary.background;
        cursor = primary.foreground;
      };

      primary = {
        background = "#1e2127";
        foreground = "#abb2bf";
      };

      selection = {
        text = primary.foreground;
        background = bright.black;
      };

      normal = {
        black = "#1e2127";
        red = "#e06c75";
        green = "#98c379";
        yellow = "#d19a66";
        blue = "#61afef";
        magenta = "#c678dd";
        cyan = "#56b6c2";
        white = "#abb2bf";
      };

      bright = {
        black = "#5c6370";
        red = "#e06c75";
        green = "#98c379";
        yellow = "#d19a66";
        blue = "#61afef";
        magenta = "#c678dd";
        cyan = "#56b6c2";
        white = "#ffffff";
      };
    };
    catppuccin-macchiato = rec {
      raw-colours = {
        rosewater = "#f4dbd6";
        flamingo = "#f0c6c6";
        pink = "#f5bde6";
        mauve = "#c6a0f6";
        red = "#ed8796";
        maroon = "#ee99a0";
        peach = "#f5a97f";
        yellow = "#eed49f";
        green = "#a6da95";
        teal = "#8bd5ca";
        sky = "#91d7e3";
        sapphire = "#7dc4e4";
        blue = "#8aadf4";
        lavender = "#b7bdf8";
        text = "#cad3f5";
        subtext1 = "#b8c0e0";
        subtext0 = "#a5adcb";
        overlay2 = "#939ab7";
        overlay1 = "#8087a2";
        overlay0 = "#6e738d";
        surface2 = "#5b6078";
        surface1 = "#494d64";
        surface0 = "#363a4f";
        base = "#24273a";
        mantle = "#1e2030";
        crust = "#181926";
      };

      primary = {
        background = raw-colours.base;
        foreground = raw-colours.text;
        dim_foreground = raw-colours.text;
        bright_foreground = raw-colours.text;
      };
      cursor = {
        text = raw-colours.base;
        cursor = raw-colours.rosewater;
      };
      vi_mode_cursor = {
        text = raw-colours.base;
        cursor = raw-colours.lavender;
      };
      search = {
        matches = {
          foreground = raw-colours.base;
          background = raw-colours.subtext0;
        };
        focused_match = {
          foreground = raw-colours.base;
          background = raw-colours.green;
        };
      };
      selection = {
        text = raw-colours.base;
        background = raw-colours.rosewater;
      };
      normal = {
        black = raw-colours.surface1;
        red = raw-colours.red;
        green = raw-colours.green;
        yellow = raw-colours.yellow;
        blue = raw-colours.blue;
        magenta = raw-colours.pink;
        cyan = raw-colours.teal;
        white = raw-colours.subtext1;
      };
      bright = {
        black = raw-colours.surface2;
        red = raw-colours.red;
        green = raw-colours.green;
        yellow = raw-colours.yellow;
        blue = raw-colours.blue;
        magenta = raw-colours.pink;
        cyan = raw-colours.teal;
        white = raw-colours.subtext0;
      };
      dim = {
        black = raw-colours.surface1;
        red = raw-colours.red;
        green = raw-colours.green;
        yellow = raw-colours.yellow;
        blue = raw-colours.blue;
        magenta = raw-colours.pink;
        cyan = raw-colours.teal;
        white = raw-colours.subtext1;
      };
    };
    catppuccin-mocha = rec {
      raw-colours = {
        rosewater = "#f5e0dc";
        flamingo = "#f2cdcd";
        pink = "#f5c2e7";
        mauve = "#cba6f7";
        red = "#f38ba8";
        maroon = "#eba0ac";
        peach = "#fab387";
        yellow = "#f9e2af";
        green = "#a6e3a1";
        teal = "#94e2d5";
        sky = "#89dceb";
        sapphire = "#74c7ec";
        blue = "#89b4fa";
        lavender = "#b4befe";
        text = "#cdd6f4";
        subtext1 = "#bac2de";
        subtext0 = "#a6adc8";
        overlay2 = "#9399b2";
        overlay1 = "#7f849c";
        overlay0 = "#6c7086";
        surface2 = "#585b70";
        surface1 = "#45475a";
        surface0 = "#313244";
        base = "#1e1e2e";
        mantle = "#181825";
        crust = "#11111b";
      };

      primary = {
        background = raw-colours.base;
        foreground = raw-colours.text;
        dim_foreground = raw-colours.text;
        bright_foreground = raw-colours.text;
      };
      cursor = {
        text = raw-colours.base;
        cursor = raw-colours.rosewater;
      };
      vi_mode_cursor = {
        text = raw-colours.base;
        cursor = raw-colours.lavender;
      };
      search = {
        matches = {
          foreground = raw-colours.base;
          background = raw-colours.subtext0;
        };
        focused_match = {
          foreground = raw-colours.base;
          background = raw-colours.green;
        };
      };
      selection = {
        text = raw-colours.base;
        background = raw-colours.rosewater;
      };
      normal = {
        black = raw-colours.surface1;
        red = raw-colours.red;
        green = raw-colours.green;
        yellow = raw-colours.yellow;
        blue = raw-colours.blue;
        magenta = raw-colours.pink;
        cyan = raw-colours.teal;
        white = raw-colours.subtext1;
      };
      bright = {
        black = raw-colours.surface2;
        red = raw-colours.red;
        green = raw-colours.green;
        yellow = raw-colours.yellow;
        blue = raw-colours.blue;
        magenta = raw-colours.pink;
        cyan = raw-colours.teal;
        white = raw-colours.subtext0;
      };
      dim = {
        black = raw-colours.surface1;
        red = raw-colours.red;
        green = raw-colours.green;
        yellow = raw-colours.yellow;
        blue = raw-colours.blue;
        magenta = raw-colours.pink;
        cyan = raw-colours.teal;
        white = raw-colours.subtext1;
      };
    };
    catppuccin-frappe = rec {
      raw-colours = {
        base = "#303446";
        blue = "#8CAAEE";
        green = "#A6D189";
        lavender = "#BABBF1";
        pink = "#F4B8E4";
        red = "#E78284";
        rosewater = "#F2D5CF";
        subtext0 = "#A5ADCE";
        subtext1 = "#B5BFE2";
        surface1 = "#51576D";
        surface2 = "#626880";
        teal = "#81C8BE";
        text = "#C6D0F5";
        yellow = "#E5C890";
      };

      primary = {
        background = raw-colours.base;
        foreground = raw-colours.text;
        dim_foreground = raw-colours.text;
        bright_foreground = raw-colours.text;
      };
      cursor = {
        text = raw-colours.base;
        cursor = raw-colours.rosewater;
      };
      vi_mode_cursor = {
        text = raw-colours.base;
        cursor = raw-colours.lavender;
      };
      search = {
        matches = {
          foreground = raw-colours.base;
          background = raw-colours.subtext0;
        };
        focused_match = {
          foreground = raw-colours.base;
          background = raw-colours.green;
        };
      };
      selection = {
        text = raw-colours.base;
        background = raw-colours.rosewater;
      };
      normal = {
        black = raw-colours.surface1;
        red = raw-colours.red;
        green = raw-colours.green;
        yellow = raw-colours.yellow;
        blue = raw-colours.blue;
        magenta = raw-colours.pink;
        cyan = raw-colours.teal;
        white = raw-colours.subtext1;
      };
      bright = {
        black = raw-colours.surface2;
        red = raw-colours.red;
        green = raw-colours.green;
        yellow = raw-colours.yellow;
        blue = raw-colours.blue;
        magenta = raw-colours.pink;
        cyan = raw-colours.teal;
        white = raw-colours.subtext0;
      };
      dim = {
        black = raw-colours.surface1;
        red = raw-colours.red;
        green = raw-colours.green;
        yellow = raw-colours.yellow;
        blue = raw-colours.blue;
        magenta = raw-colours.pink;
        cyan = raw-colours.teal;
        white = raw-colours.subtext1;
      };
    };
    nord = rec {
      fish-theme = "Nord";

      primary = {
        background = "#2e3440";
        foreground = "#d8dee9";
        dim_foreground = "#a5abb6";
      };
      cursor = {
        text = "#2e3440";
        cursor = "#d8dee9";
      };
      vi_mode_cursor = {
        text = "#2e3440";
        cursor = "#d8dee9";
      };
      selection = {
        text = primary.foreground;
        background = "#4c566a";
      };
      search = {
        matches = {
          foreground = primary.background;
          background = "#88c0d0";
        };
      };
      normal = {
        black = "#3b4252";
        red = "#bf616a";
        green = "#a3be8c";
        yellow = "#ebcb8b";
        blue = "#81a1c1";
        magenta = "#b48ead";
        cyan = "#88c0d0";
        white = "#e5e9f0";
      };
      bright = {
        black = "#4c566a";
        red = "#bf616a";
        green = "#a3be8c";
        yellow = "#ebcb8b";
        blue = "#81a1c1";
        magenta = "#b48ead";
        cyan = "#8fbcbb";
        white = "#eceff4";
      };
      dim = {
        black = "#373e4d";
        red = "#94545d";
        green = "#809575";
        yellow = "#b29e75";
        blue = "#68809a";
        magenta = "#8c738c";
        cyan = "#6d96a5";
        white = "#aeb3bb";
      };
    };
    dracula = rec {
      primary = {
        background = "#282a36";
        foreground = "#f8f8f2";
        bright_foreground = "#ffffff";
      };
      cursor = {
        text = primary.background;
        cursor = primary.foreground;
      };
      vi_mode_cursor = {
        text = primary.background;
        cursor = primary.foreground;
      };
      search = {
        matches = {
          foreground = "#44475a";
          background = "#50fa7b";
        };
        focused_match = {
          foreground = "#44475a";
          background = "#ffb86c";
        };
      };
      hints = {
        start = {
          foreground = "#282a36";
          background = "#f1fa8c";
        };
        end = {
          foreground = "#f1fa8c";
          background = "#282a36";
        };
      };
      selection = {
        text = primary.foreground;
        background = "#44475a";
      };
      normal = {
        black = "#21222c";
        red = "#ff5555";
        green = "#50fa7b";
        yellow = "#f1fa8c";
        blue = "#bd93f9";
        magenta = "#ff79c6";
        cyan = "#8be9fd";
        white = "#f8f8f2";
      };
      bright = {
        black = "#6272a4";
        red = "#ff6e6e";
        green = "#69ff94";
        yellow = "#ffffa5";
        blue = "#d6acff";
        magenta = "#ff92df";
        cyan = "#a4ffff";
        white = "#ffffff";
      };
    };

    catppuccin-latte = rec {
      fish-theme = "Snow Day";
      tmux-colour = normal.blue;
      tmux-active-pane-colour = normal.blue;
      tmux-pane-colour = normal.black;

      # Default colors
      primary = {
        background = "#EFF1F5"; # base
        foreground = "#4C4F69"; # text
        # Bright and dim foreground colors
        dim_foreground = "#4C4F69"; # text
        bright_foreground = "#4C4F69"; # text
      };

      # Cursor colors
      cursor = {
        text = "#EFF1F5"; # base
        cursor = "#DC8A78"; # rosewater
      };

      vi_mode_cursor = {
        text = "#EFF1F5"; # base
        cursor = "#7287FD"; # lavender
      };

      # Search colors
      search = {
        matches = {
          foreground = "#EFF1F5"; # base
          background = "#6C6F85"; # subtext0
        };
        focused_match = {
          foreground = "#EFF1F5"; # base
          background = "#40A02B"; # green
        };
      };

      # Keyboard regex hints
      hints = {
        start = {
          foreground = "#EFF1F5"; # base
          background = "#DF8E1D"; # yellow
        };
        end = {
          foreground = "#EFF1F5"; # base
          background = "#6C6F85"; # subtext0
        };
      };

      # Selection colors
      selection = {
        text = "#EFF1F5"; # base
        background = "#DC8A78"; # rosewater
      };

      # Normal colors
      normal = {
        black = "#5C5F77"; # subtext1
        red = "#D20F39"; # red
        green = "#40A02B"; # green
        yellow = "#DF8E1D"; # yellow
        blue = "#1E66F5"; # blue
        magenta = "#EA76CB"; # pink
        cyan = "#179299"; # teal
        white = "#ACB0BE"; # surface2
      };

      # Bright colors
      bright = {
        black = "#6C6F85"; # subtext0
        red = "#D20F39"; # red
        green = "#40A02B"; # green
        yellow = "#DF8E1D"; # yellow
        blue = "#1E66F5"; # blue
        magenta = "#EA76CB"; # pink
        cyan = "#179299"; # teal
        white = "#BCC0CC"; # surface1
      };

      # Dim colors
      dim = {
        black = "#5C5F77"; # subtext1
        red = "#D20F39"; # red
        green = "#40A02B"; # green
        yellow = "#DF8E1D"; # yellow
        blue = "#1E66F5"; # blue
        magenta = "#EA76CB"; # pink
        cyan = "#179299"; # teal
        white = "#ACB0BE"; # surface2
      };

      indexed_colors = [
        {
          index = 16;
          color = "#FE640B";
        }
        {
          index = 17;
          color = "#DC8A78";
        }
      ];
    };
    solarized-light = rec {
      primary = {
        background = "#fdf6e3";
        foreground = "#586e75";
      };

      # Normal colors
      normal = {
        black = "#073642";
        red = "#dc322f";
        green = "#859900";
        yellow = "#b58900";
        blue = "#268bd2";
        magenta = "#d33682";
        cyan = "#2aa198";
        white = "#eee8d5";
      };

      # Bright colors
      bright = {
        black = "#002b36";
        red = "#cb4b16";
        green = "#586e75";
        yellow = "#657b83";
        blue = "#839496";
        magenta = "#6c71c4";
        cyan = "#93a1a1";
        white = "#fdf6e3";
      };

      cursor = {
        text = normal.white;
        cursor = normal.black;
      };

      selection = {
        text = normal.white;
        background = normal.blue;
      };

      fish-theme = "Tomorrow";

      tmux-colour = normal.blue;
      tmux-active-pane-colour = normal.blue;
      tmux-pane-colour = normal.black;
    };
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
        {
          index = 16;
          color = "#d18616";
        }
        {
          index = 17;
          color = "#cb2431";
        }
      ];
    };
    papercolor = rec {
      # PaperColor Light 256 - alacritty color config
      # https://github.com/NLKNguyen/papercolor-theme
      # https://www.reddit.com/r/vim/comments/36xzbs/vim_paper_color_theme_inspired_by_googles/crqbfpa/
      # Default colors
      primary = {
        background = "#eeeeee";
        foreground = "#4d4d4c";
      };

      # Colors the cursor will use if `custom_cursor_colors` is true
      cursor = {
        text = "#f3f3f3";
        cursor = "#4d4d4c";
      };

      # Normal colors
      normal = {
        black = "#ededed";
        red = "#d7005f";
        green = "#718c00";
        yellow = "#d75f00";
        blue = "#4271ae";
        magenta = "#8959a8";
        cyan = "#3e999f";
        white = "#4d4d4c";
      };

      # Bright colors
      bright = {
        black = "#949494";
        red = "#d7005f";
        green = "#718c00";
        yellow = "#d75f00";
        blue = "#4271ae";
        magenta = "#8959a8";
        cyan = "#3e999f";
        white = "#f5f5f5";
      };
      selection = {
        text = normal.white;
        background = normal.blue;
      };
    };
    gruvbox = rec {
      tmux-colour = normal.yellow;

      selection = {
        text = primary.foreground;
        background = bright.black;
      };

      primary = {
        background = "#1d2021";
        foreground = "#d6cfc1";
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

      fish-theme = "Tomorrow Night Bright";
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
        {
          index = 16;
          color = "#d18616";
        }
        {
          index = 17;
          color = "#f97583";
        }
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

      fish-theme = "fish default";
    };
    poimandres = {
      # Default colors
      primary = {
        background = "#1b1e28";
        foreground = "#a6accd";
      };

      # Cursor colors
      cursor = {
        text = "CellBackground";
        cursor = "CellForeground";
      };

      # Search colors
      #
      # Colors used for the search bar and match highlighting.
      search = {
        # Allowed values are CellForeground/CellBackground, which reference the
        # affected cell, or hexadecimal colors like #ff00ff.
        matches = {
          foreground = "#1b1e28";
          background = "#add7ff";
        };

        focused_match = {
          foreground = "#1b1e28";
          background = "#add7ff";
        };
      };

      # Selection colors
      #
      # Colors which should be used to draw the selection area.
      #
      # Allowed values are CellForeground/CellBackground, which reference the
      # affected cell, or hexadecimal colors like #ff00ff.
      selection = {
        text = "CellForeground";
        background = "#303340";
      };

      # Vi mode cursor colors
      #
      # Colors for the cursor when the vi mode is active.
      #
      # Allowed values are CellForeground/CellBackground, which reference the
      # affected cell, or hexadecimal colors like #ff00ff.
      vi_mode_cursor = {
        text = "CellBackground";
        cursor = "CellForeground";
      };

      # Normal colors
      normal = {
        black = "#1b1e28";
        red = "#d0679d";
        green = "#5de4c7";
        yellow = "#fffac2";
        blue = "#89ddff";
        magenta = "#fcc5e9";
        cyan = "#add7ff";
        white = "#ffffff";
      };

      # Bright colors
      bright = {
        black = "#a6accd";
        red = "#d0679d";
        green = "#5de4c7";
        yellow = "#fffac2";
        blue = "#add7ff";
        magenta = "#fae4fc";
        cyan = "#89ddff";
        white = "#ffffff";
      };
    };
  };
  helix-theme =
    {
      github-light = "github_light";
      catppuccin-latte = "catppuccin_latte";
      catppuccin-mocha = "catppuccin_mocha";
      catppuccin-macchiato = "catppuccin_macchiato";
      nord = "nord-custom";
      papercolor = "papercolor-light";
    }
    .${cfg.theme}
    or "monokai-pro-custom";

  current-theme = themes.${cfg.theme};
  tmux-primary-colour = current-theme.tmux-colour or current-theme.normal.blue;
  tmux-background-colour = current-theme.primary.background;
  tmux-active-pane-text-colour = current-theme.tmux-active-pane-colour or current-theme.normal.blue;
  tmux-pane-text-colour = current-theme.tmux-pane-colour or current-theme.normal.white;
  fish-theme = current-theme.fish-theme or "fish default";

  bat-theme =
    {
      github-light = "GitHub";
      papercolor = "GitHub";
      solarized-light = "Solarized (light)";
      catppuccin-latte = "GitHub";
      catppuccin-mocha = "Coldark-Dark";
      catppuccin-macchiato = "Catppuccin Macchiato";
      nord = "Nord";
      monokai-pro = "Monokai Extended";
    }
    .${cfg.theme}
    or "Monokai Extended";

  is-dark-theme = builtins.elem cfg.theme dark-themes;
  papercolor-theme-slim = pkgs.vimUtils.buildVimPlugin {
    pname = "papercolor-theme-slim";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "pappasam";
      repo = "papercolor-theme-slim";
      rev = "fc105bee31207ec97c329c70a5c8cb5f793cc054";
      hash = "sha256-m/+Xsbve1fuzNEKpSK6Eddoi7gKcj04o1kSFy/H/m9w=";
    };
  };
  git-config =
    {
      catppuccin-macchiato =
        if cfg.delta.diff-so-fancy
        then {
          delta.options.diff-so-fancy = true;
        }
        else {
          includes = [
            {
              path = "${inputs.catppuccin-delta}/catppuccin.gitconfig";
            }
          ];
          delta.options.features = "catppuccin-macchiato";
        };
      catppuccin-mocha =
        if cfg.delta.diff-so-fancy
        then {
          delta.options.diff-so-fancy = true;
        }
        else {
          includes = [
            {
              path = "${inputs.catppuccin-delta}/catppuccin.gitconfig";
            }
          ];
          delta.options.features = "catppuccin-mocha";
        };
    }
    .${cfg.theme}
    or {};

  zellij-theme =
    {
      monokai-pro = "Molokai Dark";
    }
    .${cfg.theme}
    or "nord";
in
  with lib; {
    options = {
      me.theme = mkOption {
        type = types.enum (dark-themes ++ light-themes);
      };

      me.delta.diff-so-fancy = mkEnableOption "Force diff-so-fancy with delta";

      me.vscode-theme = mkOption {
        type = types.nullOr types.str;
        description = "Custom vscode theme if different";
        default = null;
      };

      me.is-dark-theme = mkOption {
        type = types.bool;
        default = is-dark-theme;
      };
    };
    config = {
      programs.bat.config.theme = bat-theme;
      programs.git = git-config;
      programs.helix.settings.theme = helix-theme;
      programs.fish.interactiveShellInit = ''
        # configure colour theme
        fish_config theme choose "${fish-theme}"
      '';
      # alacritty now cares about unused keys, so filter out any keys that alacritty does not care about
      programs.alacritty.settings.colors = attrsets.removeAttrs current-theme [
        "fish-theme"
        "tmux-active-pane-colour"
        "tmux-colour"
        "tmux-pane-colour"
        "vi_mode_cursor"
        "raw-colours"
      ];
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

        set -g status-left "#[fg=$fg_colour,bold]#S ▶"
        set -g status-right "#[fg=$fg_colour,bold]◀ #h"

        setw -g window-status-activity-style "underscore,fg=#d1d5da,bg=$bg_colour"
        setw -g window-status-separator ""
        setw -g window-status-format " #[fg=${tmux-pane-text-colour}]#I ● #F"
        setw -g window-status-current-format " #[fg=${tmux-active-pane-text-colour}]#I ● #F"
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

      programs.vscode.userSettings = {
        "workbench.colorTheme" = cfg.vscode-theme or ({}.${cfg.theme} or "Monokai Pro");
      };
      programs.gh-dash.settings.theme = {
        ui.table.showSeparator = true;
        colors = {
          text = {
            primary = current-theme.primary.foreground;
            secondary = current-theme.bright.black;
            inverted = current-theme.primary.background;
            faint = current-theme.bright.white;
            warning = current-theme.normal.red;
            success = current-theme.normal.green;
          };
          background.selected = current-theme.bright.black;
          border = {
            primary = current-theme.primary.foreground;
            secondary = current-theme.primary.foreground;
            faint = current-theme.primary.foreground;
          };
        };
      };
      programs.zellij.settings.theme = zellij-theme;
    };
  }
