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
    ./wm/river.nix
  ];

  # enable the window managers I use
  # NOTE: do not enable gnome and cinnamon as they are mutually exclusive
  # me.wm.cinnamon.enable = true;
  me.wm.bspwm = {
    enable = true;
    with-xfce = true;
  };

  me.wm.cinnamon.enable = true;

  # overrides
  services.xserver.displayManager = {
    defaultSession = lib.mkForce "cinnamon";
  };
  services.xserver.displayManager = {
    lightdm.enable = true;
    #   gdm = {
    #     enable = true;
    #     wayland = true;
    #   };
  };
}
