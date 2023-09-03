{ pkgs, ... }:
{
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 24;
        output = [
          "DP-1"
          "DP-2"
        ];
        modules-left = [ "wlr/workspaces" ];
        modules-center = [ "hyprland/window" ];
        modules-right = [ "bluetooth" ];
        # module config
        "wlr/workspaces" = {
          "on-click" = "activate";
        };
        "bluetooth" = {
          "format" = " {status}";
          "format-disabled" = "";
          "format-connected" = " {num_connections} connected";
          "tooltip-format" = "{controller_alias}\t{controller_address}";
          "tooltip-format-connected" = "{controller_alias}\t{controller_address}\n\n{device_enumerate}";
          "tooltip-format-enumerate-connected" = "{device_alias}\t{device_address}";
          "on-click" = "blueman-manager";
        };
      };
    };
  };
}
