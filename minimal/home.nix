{
  pkgs,
  lib,
  isDarwin,
  ...
}: let
  username = "simon";
  homeDir =
    if isDarwin
    then "Users"
    else "home";
  homeDirectory = "/${homeDir}/${username}";
in {
  imports = [
    ../home/bat.nix
    ../home/colours.nix
    ../home/direnv.nix
    ../home/eza.nix
    ../home/fish.nix
    ../home/font.nix
    ../home/fzf.nix
    ../home/gh.nix
    ../home/jq.nix
    ../home/nixvim
    # ../home/nix-index.nix
    ../home/tmux.nix
    ../home/git/global.nix
  ];

  options = with lib; {
    # stub options to allow for building
    me.dark-mode = mkOption {
      type = types.bool;
      default = true;
    };
  };
  config = {
    me = {
      theme = "catppuccin-macchiato";
      font-name = "JetBrains Mono";
    };
    home = {
      inherit homeDirectory username;
      stateVersion = "22.05";
      file = {
        ".bin" = {
          source = ../home/bin;
          recursive = true;
        };
      };
      packages = with pkgs; [
        git
        nix-output-monitor
      ];
    };
    xdg = {
      enable = true;
    };
  };
}
