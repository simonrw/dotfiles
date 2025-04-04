{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfg = config.me.viddy;
in {
  options.me.viddy = {
    enable = mkEnableOption "viddy";
    replace-watch = mkOption {
      type = types.bool;
      default = true;
    };
  };
  config = mkIf cfg.enable {
    home.packages = [
      pkgs.viddy
    ];
    programs.fish.shellAliases.watch = mkIf cfg.replace-watch "viddy";
  };
}
