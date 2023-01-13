{ pkgs, ... }:
let
  xfce-settings =
    {
      services.xserver.displayManager.defaultSession = "xfce";
      services.xserver.desktopManager.xfce.enable = true;

      environment.systemPackages = with pkgs; [
        xfce.xfce4-cpufreq-plugin
        xfce.xfce4-cpugraph-plugin
        xfce.xfce4-pulseaudio-plugin
        xfce.xfce4-systemload-plugin
        xfce.xfce4-xkb-plugin
      ];
    };
in
xfce-settings
