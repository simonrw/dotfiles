{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.wm.hyprland;

  defaults = config.me.defaults;
in {
  options.me.wm.hyprland = {
    enable = mkEnableOption "Hyprland";
  };
  config = mkIf cfg.enable {
    wayland.windowManager.hyprland = {
      enable = true;
      systemd.enable = true;
      settings = {
        "$mainMod" = "SUPER";

        bind = [
          "$mainMod, space, exec, wofi --show drun"
          "$mainMod, return, exec, ${defaults.browser}"
        ];
      };

      extraConfig = builtins.readFile ./hyprland/config;
    };
    home.file.".config/hypr/start.sh".source = ./hyprland/start.sh;
  };
}
