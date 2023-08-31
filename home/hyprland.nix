{ config, ... }:
{
  wayland.windowManager.hyprland = {
    enable = true;

    extraConfig = builtins.readFile ./hyprland/config;
  };
}
