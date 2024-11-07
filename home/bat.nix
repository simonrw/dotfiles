{...}: {
  programs.bat = {
    enable = true;

    config = {
      style = "plain";
    };
  };

  xdg.configFile."bat" = {
    source = ./bat;
    recursive = true;
  };
}
