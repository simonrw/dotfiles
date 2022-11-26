{ pkgs, ... }:
{
  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
    stdlib = builtins.readFile ./direnv/direnvrc;
  };
}
