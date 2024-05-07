{
  pkgs,
  lib,
  config,
  ...
}: let
  cfg = config.me.wm.hyprland;
in {
  options.me.wm.hyprland.enable = lib.mkEnableOption (lib.mdDoc "Enable Hyprland window manager");

  config = lib.mkIf cfg.enable {
    programs.hyprland = {
      enable = true;
      xwayland.enable = true;
    };
    environment.sessionVariables = {
      WLR_NO_HARDWARE_CURSORS = "1";
      NIXOS_OZONE_WL = "1";
    };
    environment.systemPackages = with pkgs; [
      xdg-desktop-portal-hyprland
      xdg-desktop-portal-gtk
      waybar
      dunst
      wofi
      hyprpaper
      wl-clipboard
      wayland-protocols
      wayland-utils
    ];
    services.dbus.enable = true;
    xdg.portal = {
      enable = true;
      wlr.enable = true;
      # extraPortals = [
      #   pkgs.xdg-desktop-portal-gtk
      # ];
    };
  };
}
