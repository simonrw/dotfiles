{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.diffview;
in {
  options.me.nixvim.diffview = {
    enable = mkEnableOption "DiffView";
  };
  config = mkIf cfg.enable {
    programs.nixvim.plugins.diffview = {
      enable = true;
    };
  };
}
