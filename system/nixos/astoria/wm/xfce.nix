{pkgs, ...}: {
  services.xserver.displayManager.defaultSession = "xfce";
  services.xserver.desktopManager.xfce.enable = true;
  services.xserver.displayManager.sddm.enable = true;
  services.picom.enable = true;

  environment.systemPackages = with pkgs; [
    gnome.adwaita-icon-theme
    gnome.gnome-tweaks
    gnomeExtensions.appindicator
    xfce.xfce4-cpufreq-plugin
    xfce.xfce4-cpugraph-plugin
    xfce.xfce4-pulseaudio-plugin
    xfce.xfce4-systemload-plugin
    xfce.xfce4-xkb-plugin
    xfce.thunar-archive-plugin
  ];
}
