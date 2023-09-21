{ pkgs, lib, ... }:
{
  imports = [
    ./wm/cinnamon.nix
    ./wm/gnome.nix
    ./wm/hyprland.nix
    ./wm/i3.nix
    ./wm/kde.nix
    ./wm/mate.nix
    ./wm/sway.nix
  ];

  # enable the window managers I use 
  # me.wm.cinnamon.enable = true;
  # me.wm.hyprland.enable = true;
  # me.wm.i3.enable = true;
  # me.wm.sway.enable = true;
  me.wm.gnome.enable = true;
  me.wm.gnome.wayland = true;

  # overrides
  # services.xserver.displayManager = {
  #   gdm.enable = lib.mkForce true;
  #   sddm.enable = lib.mkForce false;
  #   defaultSession = lib.mkForce "cinnamon";
  # };
}
