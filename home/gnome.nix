{ ... }:
{
  dconf.settings = {
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
      binding = "<Alt><Super>s";
      command = "sh -c 'wmctrl -x -a slack || slack'";
      name = "Slack";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" = {
      binding = "<Alt><Super>t";
      command = "sh -c 'wmctrl -x -a alacritty || alacritty'";
      name = "Terminal";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
      binding = "<Alt><Super>c";
      command = "sh -c 'wmctrl -x -a google-chrome || google-chrome-stable'";
      name = "Browser";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3" = {
      binding = "<Alt><Super>e";
      command = "sh -c 'wmctrl -x -a obsidian || obsidian'";
      name = "Notes";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4" = {
      binding = "<Alt><Super>r";
      command = "sh -c 'wmctrl -x -a zeal || zeal'";
      name = "Documentation";
    };
    "org/gnome/settings-daemon/plugins/media-keys" = {
      previous = [ "<Super>F7" ];
      play = [ "<Super>F8" ];
      next = [ "<Super>F9" ];
      volume-down = [ "<Super>F11" ];
      volume-up = [ "<Super>F12" ];
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
    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "advanced-alt-tab@G-dH.github.com"
        "appindicatorsupport@rgcjonas.gmail.com"
        "blur-my-shell@aunetx"
        "drive-menu@gnome-shell-extensions.gcampax.github.com"
      ];
      disabled-extensions = [ ];
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
    "org/gnome/desktop/interface" = {
      enable-hot-corners = false;
    };
    "org/gnome/mutter" = {
      workspaces-only-on-primary = false;
    };
    "org/gnome/system/location" = {
      enabled = false;
    };
    # default to dark mode for now
    # TODO: add configuration
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
    };
  };
}

