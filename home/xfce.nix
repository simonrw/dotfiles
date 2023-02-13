{ config, pkgs, ... }:
let
  mkShortcut =
    { appName
    , package ? null
    , binName ? null
    }:
    let
      binName' = if binName != null then binName else appName;
      package' = if package != null then package else pkgs.${appName};
    in
    ''${pkgs.bash}/bin/bash -c "${pkgs.wmctrl}/bin/wmctrl -x -a ${appName} || ${package'}/bin/${binName'}"'';

  theme = if config.me.dark-mode then "Adwaita-dark" else "Adwaita";

  settings = {
    xsettings = {
      "Net/ThemeName" = theme;
      "Gtk/KeyThemeName" = "Emacs";
    };
    xfce4-panel = {
      "panels/dark-mode" = config.me.dark-mode;
    };
    xfce4-keyboard-shortcuts = {
      "commands/custom/<Alt><Super>c" = (mkShortcut {
        appName = "chromium";
      });
      "commands/custom/<Alt><Super>t" = (mkShortcut {
        appName = "alacritty";
      });
      "commands/custom/<Alt><Super>e" = (mkShortcut {
        appName = "obsidian";
      });
      "commands/custom/<Alt><Super>r" = (mkShortcut {
        appName = "zeal";
      });
      "commands/custom/<Alt><Super>s" = (mkShortcut {
        appName = "slack";
      });
      "commands/custom/<Super>F12" = "${pkgs.alsaUtils}/bin/amixer sset Master playback '5%+'";
      "commands/custom/<Super>F11" = "${pkgs.alsaUtils}/bin/amixer sset Master playback '5%-'";
      "commands/custom/<Super>F9" = "${pkgs.playerctl}/bin/playerctl play-pause";
    };
  };
in
{
  xfconf.settings = settings;
}
