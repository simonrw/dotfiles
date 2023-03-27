# add symlink to nix-index database
{ lib, pkgs, config, ... }:
{
  home.file."${config.xdg.cacheHome}/nix-index/files".source = pkgs.database;
}
