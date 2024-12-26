{ config, pkgs, ... }:
{
  config = {
    home.packages = [
      pkgs.neovim
    ];

    xdg.configFile."nvim" = {
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nix-config/home/neovim";
      recursive = true;
    };
  };
}
