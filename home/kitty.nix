{ config, ... }:
let
  dark-theme = {
    foreground = "#ffffff";
    background = "#07151a";

    # The cursor color
    cursor = "#cccccc";

    # The foreground for selections
    selection_foreground = "#000000";

    # The background for selections
    selection_background = "#FFFACD";

    # Defaults
    # black
    color0 = "#616261";
    color8 = "#8d8e8d";

    # red
    color1 = "#fd8272";
    color9 = "#fec4bd";

    # green
    color2 = "#b4fa73";
    color10 = "#d6fcb9";

    # yellow
    color3 = "#fefcc3";
    color11 = "#fefdd5";

    # blue
    color4 = "#a5d5fe";
    color12 = "#c1e3fe";

    # magenta
    color5 = "#fd8ffd";
    color13 = "#fdb1fe";

    # cyan
    color6 = "#d0d1fe";
    color14 = "#e5e6fe";

    # white
    color7 = "#f1f0f2";
    color15 = "#fefffe";
  };

  light-theme = {
    # special
    foreground = "#444444";
    background = "#f7f7f7";
    cursor = "#444444";
    cursor_text_color = "background";

    # black
    color0 = "#eeeeee";
    color8 = "#bcbcbc";

    # red
    color1 = "#af0000";
    color9 = "#d70000";

    # green
    color2 = "#008700";
    color10 = "#d70087";

    # yellow
    color3 = "#5f8700";
    color11 = "#8700af";

    # blue
    color4 = "#0087af";
    color12 = "#d75f00";

    # magenta
    color5 = "#878787";
    color13 = "#d75f00";

    # cyan
    color6 = "#005f87";
    color14 = "#005faf";

    # white
    color7 = "#444444";
    color15 = "#005f87";
  };

  theme =
    if config.dark-mode
    then dark-theme
    else light-theme;

  font = config.editor-font;
in
{
  config = {
    programs.kitty = {
      enable = true;
      settings = {
        font_family = font;
        font_size = "14.0";
        background_opacity = "1.0";
        cursor_shape = "block";
        select_by_word_characters = ":@-./_~?&=%+#";
        shell = ".";
        remember_window_size = "no";
        initial_window_width = "1280";
        initial_window_height = "960";
        allow_remote_control = "no";
        term = "xterm-256color";
        window_border_width = "0";
        window_margin_width = "0";
        window_padding_width = "0";
        copy_on_select = "no";
        hide_window_decorations = "no";
        macos_thicken_font = "0.5";
        macos_option_as_alt = "yes";
        shell_integration = "disabled";
        cursor_blink_interval = "0";
      } // theme;
    };
  };
}
