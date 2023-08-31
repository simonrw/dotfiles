{ pkgs, lib, ... }:
let
  i3-settings = {
    environment.pathsToLink = [ "/libexec" ];

    services.xserver = {
      enable = true;
      desktopManager.xterm.enable = false;

      windowManager.i3 = {
        enable = true;
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [
          dmenu
          i3status
          i3lock
        ];
      };
    };

    programs.dconf.enable = true;

    environment.systemPackages = with pkgs; [

    ];
  };

  xfce-settings =
    {
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
    };

  gnome-settings = {
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
  };

  kde-settings = {
    services.xserver.desktopManager.plasma5.enable = true;
    services.xserver.displayManager.defaultSession = "plasma";
    services.xserver.displayManager.sddm.enable = true;
    programs.dconf.enable = true;
  };
  mate-settings = {
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.mate = {
      enable = true;
    };
    environment.systemPackages = [
      pkgs.gnome.gnome-themes-extra
      pkgs.mate.mate-tweak
    ];
    services.blueman.enable = true;
    environment.mate.excludePackages = with pkgs; [
      mate.mate-terminal
    ];
    services.picom.enable = true;
  };

  cinnamon-settings = {
    services.xserver.desktopManager.cinnamon.enable = true;
  };

  hyprland-settings = {
    services.xserver.displayManager.gdm.wayland = true;
    programs.hyprland = {
      enable = true;
    };
    programs.xwayland.enable = true;
    environment.sessionVariables = {
      WLR_NO_HARDWARE_CURSORS = "1";
      NIXOS_OZONE_WL = "1";
    };
    environment.systemPackages = with pkgs; [
      xdg-desktop-portal-hyprland
      waybar
      dunst
    ];
  };

  common-settings = {
    services.xserver.displayManager.gdm.enable = true;
  };
in
builtins.foldl' lib.attrsets.recursiveUpdate { } [ common-settings cinnamon-settings hyprland-settings ]
