{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.me.aws;
in
{
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
      {
        name = "plugin-aws";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-aws";
          rev = "a4cfb06627b20c9ffdc65620eb29abcedcc16340";
          hash = "sha256-bTyp5j4VcFSntJ7mJBzERgOGGgu7ub15hy/FQcffgRE=";
        };
      }
    ];
  };
}
