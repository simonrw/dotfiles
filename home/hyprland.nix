{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.wm.hyprland;

  desktop-interface =
    if config.me.dark-mode
    then {
      color-scheme = "prefer-dark";
    }
    else {
      color-scheme = "prefer-light";
    };

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
          "$mainMod, return, exec, ${defaults.terminal}"
          "$mainMod, C, killactive"
        ];
      };

      extraConfig = builtins.readFile ./hyprland/config;
    };
    programs.waybar = {
      enable = true;
      systemd.enable = true;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          height = 24;
          modules-left = [
            "hyprland/workspaces"
          ];
          modules-center = [
            "hyprland/window"
          ];
          modules-right = [
            "clock"
          ];
        };
      };
    };
    dconf.settings = {
      "org/gnome/desktop/interface" =
        {
          enable-hot-corners = false;
          font-name = "Cantarell 11";
          document-font-name = "Cantarell 11";
          monospace-font-name = "JetBrains Mono 10"; # TODO: match with global font configuration
          titlebar-font = "Cantarell Bold 11";
          font-hinting = "slight";
          font-antialiasing = "grayscale";
          icon-theme = "Adwaita";
          gtk-theme = "Adwaita";
        }
        // desktop-interface;
    };
  };
}
