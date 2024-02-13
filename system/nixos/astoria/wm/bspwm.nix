{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.wm.bspwm;
in {
  options.me.wm.bspwm.enable = mkEnableOption "BSPWM";

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      desktopManager = {
        xfce = {
          enable = true;
          noDesktop = true;
          enableXfwm = false;
        };
      };
      displayManager.defaultSession = "xfce+bspwm";
      windowManager.bspwm.enable = true;
    };
    # enable bluetooth manager
    services.blueman.enable = true;
  };
}
