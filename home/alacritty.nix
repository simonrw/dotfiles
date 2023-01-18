{ config, ... }:
let
  theme =
    if config.dark-mode then {
      primary = {
        background = "#07151a";
        foreground = "#ffffff";
      };
      normal = {
        # black:   '0x616261'
        # red:     '0xfd8272'
        # green:   '0xb4fa73'
        # yellow:  '0xfefcc3'
        # blue:    '0xa5d5fe'
        # magenta: '0xfd8ffd'
        # cyan:    '0xd0d1fe'
        # white:   '0xf1f0f2'
      };
      bright = { };
      cursor = { };
      vi_mode_cusor = { };
      selection = { };
      dim = { };
    } else {
      primary = { };
      normal = { };
      bright = { };
      cursor = { };
      vi_mode_cusor = { };
      selection = { };
      dim = { };
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
      };
    };
  };
}
