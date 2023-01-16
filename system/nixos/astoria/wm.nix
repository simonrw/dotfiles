{ pkgs, ... }:
let
  xfce-settings =
    {
      services.xserver.displayManager.defaultSession = "xfce";
      services.xserver.desktopManager.xfce.enable = true;

      environment.systemPackages = with pkgs; [
        gnome.adwaita-icon-theme
        gnome.gnome-tweaks
        gnomeExtensions.appindicator
        xfce.xfce4-cpufreq-plugin
        xfce.xfce4-cpugraph-plugin
        xfce.xfce4-pulseaudio-plugin
        xfce.xfce4-systemload-plugin
        xfce.xfce4-xkb-plugin
      ];
    };

  gnome-settings = {
    services.xserver.displayManager.defaultSession = "gnome-xorg";
    services.xserver.desktopManager.gnome.enable = true;
    services.udev.packages = with pkgs;
      [
        gnome.gnome-settings-daemon
      ];

    environment.systemPackages = with pkgs; [
      gnome.adwaita-icon-theme
      gnome.gnome-tweaks
      gnomeExtensions.appindicator
    ];
  };
in
gnome-settings
