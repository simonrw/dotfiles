{ pkgs, lib, ... }:
{
  imports = [
    ./wm/cinnamon.nix
    # ./wm/gnome.nix
    ./wm/hyprland.nix
    # ./wm/i3.nix
    # ./wm/kde.nix
    # ./wm/mate.nix
  ];

  # common settings
  config = {
    services.xserver.displayManager.gdm.enable = lib.mkForce true;
    services.xserver.displayManager.sddm.enable = lib.mkForce false;
    services.xserver.displayManager.defaultSession = lib.mkForce "cinnamon";
  };
}
