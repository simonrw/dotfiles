{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.octo-nvim;
in {
  options.me.nixvim.octo-nvim = {
    enable = mkEnableOption "octo.nvim";
  };

  config = mkIf cfg.enable {
    programs.nixvim = {
      extraPlugins = with pkgs.vimPlugins; [
        octo-nvim
      ];

      extraConfigLua = builtins.readFile ./octo-nvim-setup.lua;
    };
  };
}
