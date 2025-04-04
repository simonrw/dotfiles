{isLinux, ...}: {
  programs.kitty = {
    enable = isLinux;
    settings = {
      allow_remote_control = "no";
      background_opacity = "1.0";
      copy_on_select = "no";
      cursor_blink_interval = "0";
      cursor_shape = "block";
      hide_window_decorations = "no";
      disable_ligatures = "always";
      initial_window_height = "960";
      initial_window_width = "1280";
      macos_option_as_alt = "yes";
      macos_thicken_font = "0.5";
      remember_window_size = "no";
      select_by_word_characters = ":@-./_~?&=%+#";
      shell = ".";
      shell_integration = "disabled";
      term = "xterm-256color";
      window_border_width = "0";
      window_margin_width = "0";
      window_padding_width = "0";
    };
  };
}
