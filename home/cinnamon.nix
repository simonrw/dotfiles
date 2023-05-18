{ config, ... }:
let
  terminal = config.me.defaults.terminal;
  browser = config.me.defaults.browser;
  mkShortcutCommand = application:
    let
      app' =
        if builtins.isString application then
          { name = application; }
        else application;
    in
    "sh -c 'wmctrl -x -a ${app'.name} || ${app'.command or app'.name}'";

  # only track light themes in here since we default to dark themes
  theme-names = {
    "github-light" = "Mint-Y-Aqua";
  };
  theme = theme-names.${config.me.theme} or "Mint-Y-Dark-Aqua";
in
{
  dconf.settings = {
    "org/cinnamon/desktop/keybindings/custom-keybindings/custom0" = {
      binding = [ "<Alt><Super>s" ];
      command = mkShortcutCommand "slack";
      name = "Slack";
    };
    "org/cinnamon/desktop/keybindings/custom-keybindings/custom1" = {
      binding = [ "<Alt><Super>t" ];
      command = mkShortcutCommand terminal;
      name = "Terminal";
    };
    "org/cinnamon/desktop/keybindings/custom-keybindings/custom2" = {
      binding = [ "<Alt><Super>c" ];
      command = mkShortcutCommand browser;
      name = "Browser";
    };
    "org/cinnamon/desktop/keybindings/custom-keybindings/custom3" = {
      binding = [ "<Alt><Super>e" ];
      command = mkShortcutCommand "obsidian";
      name = "Notes";
    };
    "org/cinnamon/desktop/keybindings/custom-keybindings/custom4" = {
      binding = [ "<Alt><Super>r" ];
      command = mkShortcutCommand "zeal";
      name = "Documentation";
    };
    "org/cinnamon/desktop/keybindings" = {
      custom-list = [ "__dummy__" "custom0" "custom1" "custom2" "custom3" "custom4" ];
    };
    "org/cinnamon/desktop/wm/preferences" = {
      button-layout = "close,maximize,minimize:";
    };
    "org/cinnamon" = {
      alttab-switcher-style = "icons+preview";
    };

    # theming
    "org/cinnamon/desktop/interface" = {
      gtk-theme = theme;
      icon-theme = theme;
    };
    "org/cinnamon/theme" = {
      name = theme;
    };
  };
}

