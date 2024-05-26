{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.statusbar;
in {
  options.me.nixvim.statusbar.enable = mkEnableOption "Status bar";
  config = mkIf cfg.enable {
    programs.nixvim = {
      plugins.lualine.enable = true;
    };
  };
}
