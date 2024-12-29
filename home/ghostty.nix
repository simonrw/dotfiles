{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.ghostty;
in {
  options.me.ghostty.enable = mkEnableOption "ghostty configuration";

  config = mkIf cfg.enable {
    # not xdg as this path is fixed even on macos
    home.file.".config/ghostty/config".source = ./ghostty/config;
  };
}
