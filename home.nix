{ config, pkgs, ... }:
{
  home = {
    username = "simon";
    homeDirectory = "/Users/simon";
    stateVersion = "22.05";
  };

  programs.bat = {
    enable = true;

    config = {
      theme = "Monokai Extended";
      style = "plain";
    };
  };
  programs.home-manager.enable = true;
}
