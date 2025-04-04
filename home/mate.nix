{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.me.wm.mate;

  mkShortcutCommand = application: let
    app' =
      if builtins.isString application
      then {name = application;}
      else application;
  in "sh -c 'wmctrl -x -a ${app'.name} || ${app'.command or app'.name}'";
in {
  options.me.wm.mate = {
    enable = mkEnableOption "Mate customisations";
  };

  config.dconf.settings = mkIf cfg.enable {
    "org/mate/desktop/keybindings/custom0" = {
      binding = "<Alt><Mod4>t";
      action = mkShortcutCommand config.me.defaults.terminal;
      name = "Terminal";
    };
    "org/mate/desktop/keybindings/custom1" = {
      binding = "<Alt><Mod4>s";
      action = mkShortcutCommand "slack";
      name = "Slack";
    };
    "org/mate/desktop/keybindings/custom2" = {
      binding = "<Alt><Mod4>c";
      action = mkShortcutCommand config.me.defaults.browser;
      name = "Browser";
    };
    "org/mate/desktop/keybindings/custom3" = {
      binding = "<Alt><Mod4>e";
      action = mkShortcutCommand "obsidian";
      name = "Notes";
    };
    "org/mate/desktop/keybindings/custom4" = {
      binding = "<Alt><Mod4>r";
      action = mkShortcutCommand "zeal";
      name = "Documentation";
    };
    "org/mate/settings-daemon/plugins/media-keys" = {
      volume-up = "<Mod4>F12";
      volume-down = "<Mod4>F11";
      previous = "<Mod4>F7";
      play = "<Mod4>F8";
      next = "<Mod4>F9";
    };
  };
}
