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
          "SUPER_SHIFT, c, killactive"
          "$mainMod, c, exec, ${defaults.browser}"
          # Move focus with mainMod + arrow keys
          "$mainMod, left, movefocus, l"
          "$mainMod, right, movefocus, r"
          "$mainMod, up, movefocus, u"
          "$mainMod, down, movefocus, d"
          "$mainMod, h, movefocus, l"
          "$mainMod, l, movefocus, r"
          "$mainMod, k, movefocus, u"
          "$mainMod, j, movefocus, d"
          # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
          "SUPER_SHIFT, M, exit"

          # Switch workspaces with mainMod + [0-9]
          "$mainMod, 1, workspace, 1"
          "$mainMod, 2, workspace, 2"
          "$mainMod, 3, workspace, 3"
          "$mainMod, 4, workspace, 4"
          "$mainMod, 5, workspace, 5"
          "$mainMod, 6, workspace, 6"
          "$mainMod, 7, workspace, 7"
          "$mainMod, 8, workspace, 8"
          "$mainMod, 9, workspace, 9"
          "$mainMod, 0, workspace, 10"

          # Move active window to a workspace with mainMod + SHIFT + [0-9]
          "$mainMod SHIFT, 1, movetoworkspace, 1"
          "$mainMod SHIFT, 2, movetoworkspace, 2"
          "$mainMod SHIFT, 3, movetoworkspace, 3"
          "$mainMod SHIFT, 4, movetoworkspace, 4"
          "$mainMod SHIFT, 5, movetoworkspace, 5"
          "$mainMod SHIFT, 6, movetoworkspace, 6"
          "$mainMod SHIFT, 7, movetoworkspace, 7"
          "$mainMod SHIFT, 8, movetoworkspace, 8"
          "$mainMod SHIFT, 9, movetoworkspace, 9"
          "$mainMod SHIFT, 0, movetoworkspace, 10"

          # switch between previous workspaces
          "$mainMod, tab, focuscurrentorlast"

          # Scroll through existing workspaces with mainMod + scroll
          "$mainMod, mouse_down, workspace, e+1"
          "$mainMod, mouse_up, workspace, e-1"
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
            "wireplumber"
            "tray"
            "clock"
          ];
          "wireplumber" = {
            on-click = "pavucontrol";
          };
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
