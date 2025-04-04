{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.me.wm.gnome;
in {
  options.me.wm.gnome = {
    enable = lib.mkEnableOption (lib.mdDoc "Enable Gnome window manager");
    wayland = lib.mkEnableOption (lib.mdDoc "Enable Wayland support");
  };

  config = lib.mkIf cfg.enable {
    services.xserver.displayManager.defaultSession =
      if cfg.wayland
      then "gnome"
      else "gnome-xorg";
    services.xserver.desktopManager.gnome.enable = true;
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.displayManager.gdm.wayland = cfg.wayland;
    services.udev.packages = with pkgs; [
      gnome.gnome-settings-daemon
    ];

    environment.systemPackages = with pkgs; [
      gnome.adwaita-icon-theme
      gnome.gnome-tweaks
      gnomeExtensions.appindicator
      gnomeExtensions.advanced-alttab-window-switcher
      gnomeExtensions.just-perfection
      orchis-theme
    ];

    programs.xwayland.enable = cfg.wayland;

    # disable trackers that constantly fail
    services.gnome.tracker-miners.enable = false;
    services.gnome.tracker.enable = false;

    environment.sessionVariables = lib.mkIf cfg.wayland {
      NIXOS_OZONE_WL = "1";
    };

    services.xserver.desktopManager.gnome.extraGSettingsOverrides = ''
      [org.gnome.desktop.wm.preferences]
      resize-with-right-button=true
      mouse-button-modifier='<Alt>'
    '';
  };
}
