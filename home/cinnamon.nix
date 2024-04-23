{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.me.wm.cinnamon;

  terminal = config.me.defaults.terminal;
  browser = config.me.defaults.browser;
  mkShortcutCommand = application: let
    app' =
      if builtins.isString application
      then {name = application;}
      else application;
  in "sh -c 'wmctrl -x -a ${app'.name} || ${app'.command or app'.name}'";
in {
  options.me.wm.cinnamon = with lib; {
    enable = mkEnableOption "Cinnamon customisations";

    dark-mode = mkOption {
      type = types.bool;
      default = true;
    };
  };
  config = let
    theme =
      if cfg.dark-mode
      then "Mint-Y-Dark-Aqua"
      else "Mint-Y-Aqua";
  in
    mkIf cfg.enable
    {
      dconf.settings = {
        # custom keybindings
        "org/cinnamon/desktop/keybindings/custom-keybindings/custom0" = {
          binding = ["<Alt><Super>t"];
          command = mkShortcutCommand terminal;
          name = "Terminal";
        };
        "org/cinnamon/desktop/keybindings/custom-keybindings/custom1" = {
          binding = ["<Alt><Super>c"];
          command = mkShortcutCommand browser;
          name = "Browser";
        };
        "org/cinnamon/desktop/keybindings/custom-keybindings/custom2" = {
          binding = ["<Alt><Super>e"];
          command = mkShortcutCommand "obsidian";
          name = "Notes";
        };
        "org/cinnamon/desktop/keybindings/custom-keybindings/custom3" = {
          binding = ["<Alt><Super>r"];
          command = mkShortcutCommand "zeal";
          name = "Documentation";
        };
        "org/cinnamon/desktop/keybindings/custom-keybindings/custom4" = {
          binding = ["<Super>e"];
          command = "dbus-send --dest=org.Cinnamon --print-reply /org/Cinnamon org.Cinnamon.ShowExpo";
          name = "Expo";
        };
        "org/cinnamon/desktop/keybindings/custom-keybindings/custom5" = {
          binding = ["<Alt><Super>s"];
          command = mkShortcutCommand "slack";
          name = "Slack";
        };
        "org/cinnamon/desktop/keybindings" = {
          custom-list = ["__dummy__" "custom0" "custom1" "custom2" "custom3" "custom4" "custom5"];
        };
        # media keys
        "org/cinnamon/desktop/keybindings/media-keys" = {
          previous = ["XF86AudioPrev" "<Super>F7"];
          play = ["XF86AudioPlay" "<Super>F8"];
          next = ["XF86AudioNext" "<Super>F9"];
          mute = ["XF86AudioMute" "<Super>F10"];
          volume-down = ["XF86AudioLowerVolume" "<Super>F11"];
          volume-up = ["XF86AudioRaiseVolume" "<Super>F12"];
        };
        # other
        "org/cinnamon/desktop/wm/preferences" = {
          button-layout = "close,maximize,minimize:";
          titlebar-font = "Ubuntu Bold 10";
        };
        "org/cinnamon" = {
          alttab-switcher-style = "icons+preview";
          desktop-effects-workspace = true;
        };
        "org/cinnamon/desktop/background" = {
          picture-uri = "file://${./apple.png}";
          picture-uri-dark = "file://${./apple.png}";
        };
        "org/cinnamon/desktop/keybindings/wm" = {
          toggle-fullscreen = ["<Super>0"];
        };
        # theming
        "org/x/apps/portal" = {
          color-scheme =
            if cfg.dark-mode
            then "prefer-dark"
            else "prefer-light";
        };
        "org/cinnamon/desktop/interface" = {
          gtk-theme = theme;
          icon-theme = "Mint-Y-Sand";
          font-name = "Ubuntu Medium 10";
        };
        "org/gnome/desktop/interface" = mkForce {
          gtk-theme = theme;
          icon-theme = "Mint-Y-Sand";
          font-name = "Ubuntu Medium 10";
        };
        "org/nemo/desktop" = {
          font = "Ubuntu Medium 10";
        };
        "org/cinnamon/theme" = {
          name = theme;
        };
      };
    };
}
