{ config, pkgs, ... }:
let
  settings = {
    xfce4-panel = {
      "panels/dark-mode" = config.dark-mode;
    };
    xfce4-keyboard-shortcuts = {
      "commands/custom/<Alt><Super>c" = ''${pkgs.wmctrl}/bin/wmctrl -x -a google-chrome || ${pkgs.google-chrome}/bin/google-chrome-stable'';
    };
  };
in
{
  xfconf.settings =
    if pkgs.stdenv.isLinux then settings else null;
}
