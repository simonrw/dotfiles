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

  me.wm.gnome = {
    enable = true;
    wayland = false;
  };

  # overrides
  services.xserver.displayManager = {
    gdm = {
      enable = true;
      wayland = false;
    };
  };
}
