{
  lib,
  pkgs,
  config,
  ...
}: let
  cfg = config.me.wm.cinnamon;
in {
  options.me.wm.cinnamon = {
    enable = lib.mkEnableOption (lib.mdDoc "Enable Cinnamon window manager");
  };

  config = lib.mkIf cfg.enable {
    services.xserver.desktopManager.cinnamon.enable = true;
    services.gvfs.enable = true;
    environment.cinnamon.excludePackages = with pkgs; [
      orca
    ];
  };
}
