{ config, lib, ... }:
let
  themes = {
    gruvbox = {
      primary = {
        background = "0x1d2021";
        foreground = "0xd5c4a1";
      };

      # Colors the cursor will use if `custom_cursor_colors` is true
      cursor = {
        text = "0x1d2021";
        cursor = "0xd5c4a1";
      };

      # Normal colors
      normal = {
        black = "0x1d2021";
        red = "0xfb4934";
        green = "0xb8bb26";
        yellow = "0xfabd2f";
        blue = "0x83a598";
        magenta = "0xd3869b";
        cyan = "0x8ec07c";
        white = "0xd5c4a1";

      };
      # Bright colors
      bright = {
        black = "0x665c54";
        red = "0xfe8019";
        green = "0x3c3836";
        yellow = "0x504945";
        blue = "0xbdae93";
        magenta = "0xebdbb2";
        cyan = "0xd65d0e";
        white = "0xfbf1c7";
      };
    };
    github = {
      # github Alacritty Colors
      # Default colors
      primary = {
        background = "#24292e";
        foreground = "#edf0f2";
      };

      # Normal colors
      normal = {
        black = "0x586069";
        red = "0xea4a5a";
        green = "0x34d058";
        yellow = "0xffea7f";
        blue = "0x5f9fe8";
        magenta = "0xb392f0";
        cyan = "0x39c5cf";
        white = "0xd1d5da";
      };

      # Bright colors
      bright = {
        black = "0x959da5";
        red = "0xf97583";
        green = "0x85e89d";
        yellow = "0xffea7f";
        blue = "0x79b8ff";
        magenta = "0xb392f0";
        cyan = "0x56d4dd";
        white = "0xfafbfc";
      };

      indexed_colors = [
        { index = 16; color = "0xd18616"; }
        { index = 17; color = "0xf97583"; }
      ];
    };
  };
  neovim-theme-blocks = {
    github = ''
      set background=dark
      colorscheme github_dark
      " overrides
      highlight Comment guifg=#e69340   " brighten comments
    '';
    gruvbox = ''
      set background=dark
      colorscheme base16-gruvbox-dark-hard
    '';
  };
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
    # TODO: tmux theme
  };
}
