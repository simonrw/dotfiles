{ pkgs, lib, isDarwin, ... }:
let
  username = "simon";
  homeDir = if isDarwin then "Users" else "home";
  homeDirectory = "/${homeDir}/${username}";
in
{
  imports = [
    ../home/colours.nix
    ../home/fish.nix
    ../home/font.nix
    ../home/neovim.nix
    ../home/tmux.nix
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
      theme = "nord";
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
      ];
    };
    xdg = {
      enable = true;
      configFile.nvim = {
        source = ../home/nvim;
        recursive = true;
      };
    };
  };
}
