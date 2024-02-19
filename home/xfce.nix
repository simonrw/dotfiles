{
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  cfg = config.me.wm.xfce;

  mkShortcut = {
    appName,
    package ? null,
    binName ? null,
  }: let
    binName' =
      if binName != null
      then binName
      else appName;
    package' =
      if package != null
      then package
      else pkgs.${appName};
  in ''bash -c "${pkgs.wmctrl}/bin/wmctrl -x -a ${appName} || ${package'}/bin/${binName'}"'';

  theme =
    if config.me.dark-mode
    then "Adwaita-dark"
    else "Adwaita";
in {
  options.me.wm.xfce = {
    enable = mkEnableOption "Cinnamon customisations";
  };

  config.xfconf.settings = mkIf cfg.enable {
    xsettings = {
      "Net/ThemeName" = theme;
      "Gtk/KeyThemeName" = "";
    };
    xfce4-panel = {
      "panels/dark-mode" = config.me.dark-mode;
    };
    xfce4-keyboard-shortcuts = {
      "commands/custom/<Alt><Super>c" = mkShortcut {
        appName = config.me.defaults.browser;
      };
      "commands/custom/<Alt><Super>t" = mkShortcut {
        appName = config.me.defaults.terminal;
      };
      "commands/custom/<Alt><Super>e" = mkShortcut {
        appName = "obsidian";
      };
      "commands/custom/<Alt><Super>r" = mkShortcut {
        appName = "zeal";
      };
      "commands/custom/<Alt><Super>s" = mkShortcut {
        appName = "slack";
      };
      "commands/custom/<Super>F12" = ''bash -c "${pkgs.alsaUtils}/bin/amixer sset Master playback '5%+'"'';
      "commands/custom/<Super>F11" = ''bash -c "${pkgs.alsaUtils}/bin/amixer sset Master playback '5%-'"'';
      "commands/custom/<Super>F9" = ''bash -c "${pkgs.playerctl}/bin/playerctl play-pause"'';
      "xfwm4/custom/<Super>Left" = "tile_left_key";
      "xfwm4/custom/<Super>Right" = "tile_right_key";
    };
  };
}
