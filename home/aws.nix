{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.me.aws;
in {
  options.me.aws = {
    enable = mkEnableOption "awscli";

    package = mkOption {
      type = types.package;
      default = pkgs.awscli2;
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      cfg.package
    ];
  };
}
