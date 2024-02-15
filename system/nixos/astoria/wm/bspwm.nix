{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.wm.bspwm;
in {
  options.me.wm.bspwm = {
    enable = mkEnableOption "BSPWM";

    with-xfce = mkOption {
      type = types.bool;
      default = false;
      description = "Use xfce as a desktop environment";
    };
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      desktopManager = {
        xfce = {
          enable = cfg.with-xfce;
          noDesktop = true;
          enableXfwm = false;
        };
      };
      displayManager.defaultSession = "none+bspwm";
      windowManager.bspwm.enable = true;
    };
    # enable bluetooth manager
    services.blueman.enable = true;
  };
}
