{ pkgs, ... }:
{
  programs.kitty = {
    enable = !pkgs.kitty.meta.broken;
    settings = {
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
      disable_ligatures = "always";
    };
  };
}
