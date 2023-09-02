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
        modules-center = [ ];
        modules-right = [ ];
      };
    };
  };
}
