{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.zellij;
in {
  options.me.zellij = {
    enable = mkEnableOption "zellij";
    enableFishIntegration = mkEnableOption "zellij fish integration";
  };

  config = mkIf cfg.enable {
    programs.zellij = {
      enable = true;
      enableFishIntegration = cfg.enableFishIntegration;
    };

    xdg.configFile."zellij" = {
      source = ./zellij;
      recursive = true;
    };
  };
}
