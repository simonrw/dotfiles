{ config, pkgs, ... }:
{
  xfconf.settings =
    if pkgs.stdenv.isLinux then {
      xfce4-panel = {
        "panels/dark-mode" = config.dark-mode;
      };
    } else null;
}
