{config, ...}: {
  programs.bat = {
    enable = true;

    config = {
      style = "plain";
    };
  };

  xdg.configFile."bat" = {
    source = config.lib.file.mkOutOfStoreSymlink ./bat;
    recursive = true;
  };
}
