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
  me.wm.cinnamon.enable = true;
  me.wm.sway.enable = true;

  # overrides
  services.xserver.displayManager = {
    gdm.enable = lib.mkForce false;
    sddm.enable = lib.mkForce true;
    defaultSession = lib.mkForce "cinnamon";
  };
}
