{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.wm.kde;
in {
  options.me.wm.kde.enable = mkEnableOption "KDE";

  config = mkIf cfg.enable {
    services.kdeconnect.enable = true;
    programs.chromium.extensions = [
      "cimiefiiaegbelhefglklhhakcgmhkai" # plasma integration
    ];
    home.packages = [
      pkgs.kmail
    ];
  };
}
