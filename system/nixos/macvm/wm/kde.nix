{
  lib,
  config,
  ...
}: let
  cfg = config.me.wm.kde;
in {
  options.me.wm.kde.enable = lib.mkEnableOption "KDE";

  config = lib.mkIf cfg.enable {
    services.xserver.desktopManager.plasma5.enable = true;
    services.xserver.displayManager.defaultSession = "plasma";
    services.xserver.displayManager.sddm.enable = true;
    programs.dconf.enable = true;
  };
}
