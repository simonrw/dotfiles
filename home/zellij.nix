{...}: {
  programs.zellij = {
    enable = true;
    enableFishIntegration = true;
  };

  xdg.configFile."zellij" = {
    source = ./zellij;
    recursive = true;
  };
}
