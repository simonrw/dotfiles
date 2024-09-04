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
    programs.fish.plugins = mkIf config.programs.fish.enable [
      # my fork to stop printing on error when aws_completer cannot be found
      {
        name = "plugin-aws";
        src = pkgs.fetchFromGitHub {
          owner = "simonrw";
          repo = "plugin-aws";
          rev = "bdbf81a1260ace6bcafd129dcfef2c628a937925";
          hash = "sha256-z6hFVk3H5vwxqwolIqWSO8/73hiCHClJeA4gNZw7B4Q=";
        };
      }
    ];
  };
}
