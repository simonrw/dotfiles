{ pkgs, ... }:
{
  services.xserver.displayManager.defaultSession = "gnome-xorg";
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = false;
  services.udev.packages = with pkgs;
    [
      gnome.gnome-settings-daemon
    ];

  environment.systemPackages = with pkgs; [
    gnome.adwaita-icon-theme
    gnome.gnome-tweaks
    gnomeExtensions.appindicator
    gnomeExtensions.advanced-alttab-window-switcher
    orchis-theme
  ];

  programs.xwayland.enable = false;

  services.xserver.desktopManager.gnome.extraGSettingsOverrides = ''
    [org.gnome.desktop.wm.preferences]
    resize-with-right-button=true
    mouse-button-modifier='<Alt>'
  '';
}
