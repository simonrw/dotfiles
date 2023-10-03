{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.contour;
  yamlFormat = pkgs.formats.yaml { };
in
{
  options = {
    programs.contour = {
      enable = mkEnableOption "Contour";

      package = mkOption {
        type = types.package;
        default = pkgs.contour;
        defaultText = literalExpression "pkgs.contour";
        description = "The Contour package to install";
      };

      settings = mkOption {
        type = yamlFormat.type;
        default = { };
        example = literalExpression ''
          TODO
        '';
        description = ''
          TODO
        '';
      };
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      home.packages = [ cfg.package ];

      xdg.configFile."contour/contour.yml" = {
        source = yamlFormat.generate "contour.yml" cfg.settings;
      };
    })
  ];
}
