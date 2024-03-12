{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.me.wm.river;
in {
  options.me.wm.river = {
    enable = mkEnableOption "river";
  };

  config = mkIf cfg.enable {
    wayland.windowManager.river = {
      enable = true;
      settings = {
        keyboard-layout = "gb";
      };
    };
  };
}
