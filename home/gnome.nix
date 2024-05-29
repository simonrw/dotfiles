{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  terminal = config.me.defaults.terminal;
  browser = config.me.defaults.browser;
  mkShortcutCommand = application: let
    app' =
      if builtins.isString application
      then {name = application;}
      else application;
  in
    if cfg.wayland
    then "sh -c '${pkgs.wlman}/bin/wlman ${app'.name} || ${app'.command or app'.name}'"
    else "sh -c 'wmctrl -x -a ${app'.name} || ${app'.command or app'.name}'";

  animation-speed-integer =
    {
      "none" = false;
      "default" = 1;
      "fast" = 4;
      "faster" = 3;
      "fastest" = 2;
    }
    .${cfg.animation-speed};

  desktop-interface =
    if config.me.dark-mode
    then {
      color-scheme = "prefer-dark";
    }
    else {
      color-scheme = "prefer-light";
    };

  cfg = config.me.wm.gnome;
  wallpaper = config.me.wallpaper;
in {
  options.me.wm.gnome = {
    enable = mkEnableOption (mdDoc "Enable gnome configuration");
    animation-speed = mkOption {
      type = types.enum [
        "none"
        "default"
        "fast"
        "faster"
        "fastest"
      ];
      default = "faster";
    };
    wayland = mkEnableOption "Wayland support";
  };

  config = mkIf cfg.enable {
    dconf.settings = {
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
        binding = "<Alt><Super>s";
        command = mkShortcutCommand "slack";
        name = "Slack";
      };
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" = {
        binding = "<Alt><Super>t";
        command = mkShortcutCommand terminal;
        name = "Terminal";
      };
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
        binding = "<Alt><Super>c";
        command = mkShortcutCommand browser;
        name = "Browser";
      };
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3" = {
        binding = "<Alt><Super>e";
        command = mkShortcutCommand "obsidian";
        name = "Notes";
      };
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4" = {
        binding = "<Alt><Super>r";
        command = mkShortcutCommand "zeal";
        name = "Documentation";
      };
      "org/gnome/settings-daemon/plugins/media-keys" = {
        previous = ["<Super>F7"];
        play = ["<Super>F8"];
        next = ["<Super>F9"];
        volume-down = ["<Super>F11"];
        volume-up = ["<Super>F12"];
      };
      "org/gnome/settings-daemon/plugins/media-keys" = {
        screenreader = [];
        magnifier = [];
        custom-keybindings = [
          "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
          "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/"
          "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/"
          "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/"
          "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4/"
        ];
      };
      "org/gnome/shell/extensions/just-perfection" = {
        animation = animation-speed-integer;
      };
      "org/gnome/shell" = {
        disable-user-extensions = false;
        enabled-extensions = [
          "advanced-alt-tab@G-dH.github.com"
          "appindicatorsupport@rgcjonas.gmail.com"
          "drive-menu@gnome-shell-extensions.gcampax.github.com"
          "user-theme@gnome-shell-extensions.gcampax.github.com"
          "just-perfection-desktop@just-perfection"
          "window-calls-extended@hseliger.eu"
          "activate-window-by-title@lucaswerkmeister.de"
        ];
        disabled-extensions = [];
      };
      # search result options
      "org/gnome/desktop/search-providers" = {
        disabled = [
          "org.gnome.Photos.desktop"
          "org.gnome.Contacts.desktop"
          "org.gnome.clocks.desktop"
          "org.gnome.Epiphany.desktop"
        ];
      };
      "org/gnome/desktop/interface" =
        {
          enable-hot-corners = false;
          font-name = "Cantarell 11";
          document-font-name = "Cantarell 11";
          monospace-font-name = "JetBrains Mono 10"; # TODO: match with global font configuration
          titlebar-font = "Cantarell Bold 11";
          font-hinting = "slight";
          font-antialiasing = "grayscale";
          icon-theme = "Adwaita";
          gtk-theme = "Adwaita";
          cursor-theme = "Adwaita";
        }
        // desktop-interface;
      "org/gnome/desktop/background" = {
        picture-uri = "file://${wallpaper}";
        picture-uri-dark = "file://${wallpaper}";
      };
      "org/gnome/mutter" = {
        workspaces-only-on-primary = false;
      };
      "org/gnome/system/location" = {
        enabled = false;
      };
      "org/gnome/desktop/wm/preferences" = {
        mouse-button-modifier = "<Alt>";
        focus-mode = "click";
        # apple style button layout
        button-layout = "close,minimize,maximize:";
      };
      "org/gnome/desktop/peripherals/mouse" = {
        accel-profile = "flat";
        gtk-enable-primary-paste = false;
      };
      "org/gnome/desktop/sound" = {
        allow-volume-above-100-percent = true;
      };
      "org/gnome/shell/extensions/user-theme" = {
        name = "";
      };
    };
  };
}
