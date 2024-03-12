{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.me.wm.river;
in {
  options.me.wm.river = {
    enable = mkEnableOption "river";
  };

  config = mkIf cfg.enable {
    programs.river = {
      enable = true;

      extraPackages = with pkgs; [
        rofi
        swaylock
        foot
      ];
    };
  };
}
