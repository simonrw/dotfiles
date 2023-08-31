{ config, ... }:
{
  wayland.windowManager.hyprland = {
    enable = true;

    extraConfig = ''
    $mod = SUPER
    '';
  };
}
