{
  pkgs,
  lib,
  config,
  ...
}: let
  cfg = config.me.wm.mate;
in {
  options.me.wm.mate.enable = lib.mkEnableOption "MATE";

  config = lib.mkIf cfg.enable {
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.mate = {
      enable = true;
    };
    environment.systemPackages = [
      pkgs.gnome.gnome-themes-extra
      pkgs.mate.mate-tweak
    ];
    environment.mate.excludePackages = with pkgs; [
      mate.mate-terminal
    ];
    services.picom.enable = true;
  };
}
