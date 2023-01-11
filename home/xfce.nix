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
    ''sh -c "${pkgs.wmctrl}/bin/wmctrl -x -a ${appName} || ${package'}/bin/${binName'}"'';

  theme = if config.dark-mode then "Adwaita-dark" else "Adwaita";

  settings = {
    xsettings = {
      "Net/ThemeName" = theme;
    };
    xfce4-panel = {
      "panels/dark-mode" = config.dark-mode;
    };
    xfce4-keyboard-shortcuts = {
      "commands/custom/<Alt><Super>c" = (mkShortcut {
        appName = "firefox";
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
    };
  };
in
{
  xfconf.settings = settings;
}
