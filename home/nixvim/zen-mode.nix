{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.zen-mode;
in {
  options = {
    me.nixvim.zen-mode.enable = mkEnableOption "Zen mode";
  };
  config = mkIf cfg.enable {
    extraPlugins = with pkgs.vimPlugins; [
      zen-mode-nvim
    ];
  };
}
