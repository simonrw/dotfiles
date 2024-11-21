{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.hardtime;
in {
  options.me.nixvim.hardtime = {
    enable = mkEnableOption "hardtime";
  };

  config = mkIf cfg.enable {
    plugins.hardtime = {
      enable = true;
      settings = {
        allow_different_key = true;
        disable_mouse = false;
      };
    };
  };
}
