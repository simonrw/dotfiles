{config, ...}: let
  dark-theme-value =
    if config.me.is-dark-theme
    then "1"
    else "0";
in {
  gtk = {
    enable = true;
    gtk2 = {
      configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
      extraConfig = ''
        gtk-application-prefer-dark-theme=${dark-theme-value}
      '';
    };
    gtk3 = {
      extraConfig = {
        gtk-application-prefer-dark-theme = dark-theme-value;
      };
    };
    gtk4 = {
      extraConfig = {
        gtk-application-prefer-dark-theme = dark-theme-value;
      };
    };
  };
}
