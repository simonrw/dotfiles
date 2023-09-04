{ pkgs, lib, ... }:
{
  imports = [
    ./wm/cinnamon.nix
    ./wm/gnome.nix
    ./wm/hyprland.nix
    ./wm/i3.nix
    ./wm/kde.nix
    ./wm/mate.nix
  ];

  # enable the window managers I use 
  me.wm.cinnamon.enable = true;
  me.wm.hyprland.enable = true;

  # overrides
  services.xserver.displayManager = {
    gdm.enable = lib.mkForce true;
    sddm.enable = lib.mkForce false;
    defaultSession = lib.mkForce "cinnamon";
  };
}
