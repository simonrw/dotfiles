{ pkgs, lib, ... }:
{
  imports = [
    ./wm/cinnamon.nix
    ./wm/gnome.nix
    ./wm/i3.nix
    # ./wm/kde.nix
    ./wm/mate.nix
    ./wm/sway.nix
    ./wm/bspwm.nix
    ./wm/pantheon.nix
  ];

  # enable the window managers I use 
  # me.wm.cinnamon.enable = true;
  me.wm.gnome = {
    enable = true;
    wayland = false;
  };
  # me.wm.sway.enable = true;

  # overrides
  # services.xserver.displayManager = {
  #   gdm.enable = lib.mkForce true;
  #   sddm.enable = lib.mkForce false;
  #   defaultSession = lib.mkForce "cinnamon";
  # };
}
