{ config, ... }:
{
  config = {
    programs.exa = {
      enable = true;
      enableAliases = true;
    };
  };
}
