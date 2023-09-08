{ pkgs, config, ... }:
{
  config = {
    programs.exa = {
      enable = true;
      package = pkgs.eza;
      enableAliases = true;
    };
  };
}
