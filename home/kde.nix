{ pkgs, ... }:
{
  config = {
    services.kdeconnect.enable = true;
    programs.chromium.extensions = [
      "cimiefiiaegbelhefglklhhakcgmhkai" # plasma integration
    ];
    home.packages = [
      pkgs.kmail
    ];
  };
}
