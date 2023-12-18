{ lib, config, ... }:
with lib;
let
  cfg = config.me.wm.hyprland;
in
{
  options.me.wm.hyprland = {
    enable = mkEnableOption "Hyprland";
  };
  config = mkIf cfg.enable {
    wayland.windowManager.hyprland = {
      enable = true;

      extraConfig = builtins.readFile ./hyprland/config;
    };
  };
}
