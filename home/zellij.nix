{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.zellij;
in {
  options.me.zellij.enable = mkEnableOption "zellij";

  config = mkIf cfg.enable {
    programs.zellij = {
      enable = true;
      enableFishIntegration = true;
    };

    xdg.configFile."zellij" = {
      source = ./zellij;
      recursive = true;
    };
  };
}
