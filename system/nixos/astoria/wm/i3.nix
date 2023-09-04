{ pkgs, lib, config, ... }:
let
  cfg = config.me.wm.i3;
in
{
  options.me.wm.i3.enable = lib.mkEnableOption (lib.mkDoc "Enable i3 support");

  config = lib.mkIf cfg.enable {
    environment.pathsToLink = [ "/libexec" ];

    services.xserver = {
      enable = true;
      desktopManager.xterm.enable = false;

      windowManager.i3 = {
        enable = true;
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [
          dmenu
          i3status
          i3lock
        ];
      };
    };

    programs.dconf.enable = true;

    environment.systemPackages = with pkgs; [ ];
  };
}
