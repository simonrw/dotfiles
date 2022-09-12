{ config, pkgs, ... }:
{
  home = {
    username = "simon";
    homeDirectory = "/Users/simon";
    stateVersion = "22.05";

    sessionVariables = {
      LANG = "en_GB.UTF-8";
      LC_CTYPE = "en_GB.UTF-8";
      LC_ALL = "en_GB.UTF-8";
      EDITOR = "nvim";
      PAGER = "bat";
      MANPAGER = "sh -c 'col -bx | ${pkgs.bat}/bin/bat -l man -p'";
    };
  };

  programs.bat = {
    enable = true;

    config = {
      theme = "Monokai Extended";
      style = "plain";
    };
  };

  programs.home-manager.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };

}
