{ config, ... }:
let
  theme =
    if config.dark-mode then {
      # github Alacritty Colors
      # Default colors
      primary = {
        background = "0x24292e";
        foreground = "0xd1d5da";
      };

      # Normal colors
      normal = {
        black = "0x586069";
        red = "0xea4a5a";
        green = "0x34d058";
        yellow = "0xffea7f";
        blue = "0x2188ff";
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
    } else {
      primary = {
        foreground = "#444444";
        background = "#f7f7f7";
      };
      normal = {
        black = "0xeeeeee";
        red = "0xaf0000";
        green = "0x008700";
        yellow = "0x5f8700";
        blue = "0x0087af";
        magenta = "0x878787";
        cyan = "0x005f87";
        white = "0x444444";
      };
      bright = {
        black = "0xbcbcbc";
        red = "0xd70000";
        green = "0xd70087";
        yellow = "0x8700af";
        blue = "0xd75f00";
        magenta = "0xfdb1fe";
        cyan = "0x005faf";
        white = "0x005f87";
      };
      cursor = {
        text = "#eeeeee";
        cursor = "#444444";
      };
      vi_mode_cusor = {
        text = "#eeeeee";
        cursor = "#444444";
      };
    };
in
{
  config = {
    programs.alacritty = {
      enable = true;
      settings = {
        env = {
          "TERM" = "xterm-256color";
        };
        window.dimensions = {
          columns = 120;
          lines = 40;
        };
        font.size = 12.0;
        font.normal = {
          family = config.editor-font;
          style = "Regular";
        };
        font.italic = {
          family = config.editor-font;
          style = "Regular";
        };
        colors = theme;
        "key_bindings" = [
          {
            key = "F";
            mods = "Alt";
            chars = "\\x1bf";
          }
          {
            key = "B";
            mods = "Alt";
            chars = "\\x1bb";
          }
          {
            key = "D";
            mods = "Alt";
            chars = "\\x1bd";

          }
          {
            key = "N";
            mods = "Command";
            action = "SpawnNewInstance";
          }
        ];
      };
    };
  };
}
