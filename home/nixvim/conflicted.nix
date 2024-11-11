{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfg = config.me.nixvim.conflicted;

  vim-conflicted = pkgs.vimUtils.buildVimPlugin {
    name = "vim-conflicted";
    src = pkgs.fetchFromGitHub {
      owner = "christoomey";
      repo = "vim-conflicted";
      rev = "068c320796f807ac4961618e3e62316773803996";
      hash = "sha256-x7tkN71gbnt1HI6NQQNF3e7tGSUbkltNN4nbV6XunNM=";
    };
  };
in {
  options.me.nixvim.conflicted = {
    enable = mkEnableOption "vim-conflicted";
  };

  config = mkIf cfg.enable {
    extraPlugins = [
      vim-conflicted
    ];
  };
}
