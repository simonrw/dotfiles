{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.me.wm.pantheon;
in
{
  options.me.wm.pantheon = {
    enable = mkEnableOption "Pantheon";

    disableApps = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    services.xserver.desktopManager.pantheon.enable = true;
    services.pantheon.apps.enable = !cfg.disableApps;
    services.xserver.displayManager.lightdm.greeters.pantheon.enable = true;
    environment.pantheon.excludePackages = with pkgs; [
      orca
    ];
  };
}
