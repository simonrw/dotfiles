{ config, ... }:
let
  theme =
    if config.dark-mode then {
      primary = {
        background = "#07151a";
        foreground = "#ffffff";
      };
      normal = {
        black = "0x616261";
        red = "0xfd8272";
        green = "0xb4fa73";
        yellow = "0xfefcc3";
        blue = "0xa5d5fe";
        magenta = "0xfd8ffd";
        cyan = "0xd0d1fe";
        white = "0xf1f0f2";
      };
      bright = {
        black = "0x8d8e8d";
        red = "0xfec4bd";
        green = "0xd6fcb9";
        yellow = "0xfefdd5";
        blue = "0xc1e3fe";
        magenta = "0xfdb1fe";
        cyan = "0xe5e6fe";
        white = "0xfefffe";
      };
      cursor = {
        text = "#000000";
        cursor = "#ffffff";
      };
      vi_mode_cusor = {
        text = "#000000";
        cursor = "#ffffff";
      };
      selection = {
        text = "#eaeaea";
        background = "#404040";
      };
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
