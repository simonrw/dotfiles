{ lib, isDarwin, ... }:
let
  username = "simon";
  homeDir = if isDarwin then "Users" else "home";
  homeDirectory = "/${homeDir}/${username}";
in
{
  imports = [
    ../home/neovim.nix
    ../home/fish.nix
  ];

  options = with lib; {
    # stub options to allow for building
    me.dark-mode = mkOption {
      type = types.bool;
      default = true;
    };
  };
  config = {
    home = {
      inherit homeDirectory username;
      stateVersion = "22.05";
    };
    home.file = {
      ".bin" = {
        source = ../home/bin;
        recursive = true;
      };
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
