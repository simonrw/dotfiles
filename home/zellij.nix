{...}: {
  programs.zellij = {
    enable = true;
    enableFishIntegration = true;
    settings = {
    };
  };

  xdg.configFile."zellij/layouts" = {
    source = ./zellij/layouts;
    recursive = true;
  };
}
