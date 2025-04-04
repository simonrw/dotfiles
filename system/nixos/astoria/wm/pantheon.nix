{
  lib,
  pkgs,
  config,
  ...
}:
with lib; let
  cfg = config.me.wm.pantheon;
in {
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
    programs.pantheon-tweaks.enable = true;
    services = {
      gnome.gnome-keyring.enable = true;
      gvfs.enable = true;
    };
    # https://github.com/NixOS/nixpkgs/issues/144045#issuecomment-992487775
    services.xserver.desktopManager.pantheon.extraWingpanelIndicators = with pkgs; [wingpanel-indicator-ayatana];
    systemd.user.services.indicatorapp = {
      description = "indicator-application-gtk3";
      wantedBy = ["graphical-session.target"];
      partOf = ["graphical-session.target"];
      serviceConfig = {
        ExecStart = "${pkgs.indicator-application-gtk3}/libexec/indicator-application/indicator-application-service";
      };
    };
  };
}
