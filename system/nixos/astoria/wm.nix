{lib, ...}: {
  imports = [
    ./wm/cinnamon.nix
    ./wm/gnome.nix
    ./wm/i3.nix
    # ./wm/kde.nix
    ./wm/mate.nix
    ./wm/sway.nix
    ./wm/bspwm.nix
    ./wm/pantheon.nix
    ./wm/hyprland.nix
  ];

  # enable the window managers I use
  # NOTE: do not enable gnome and cinnamon as they are mutually exclusive
  me.wm.bspwm.enable = true;
  me.wm.gnome = {
    enable = true;
    wayland = true;
  };

  # overrides
  services.xserver.displayManager.defaultSession = lib.mkForce "gnome";
  # services.xserver.displayManager = {
  #   gdm.enable = lib.mkForce true;
  #   sddm.enable = lib.mkForce false;
  #   defaultSession = lib.mkForce "cinnamon";
  # };
}
