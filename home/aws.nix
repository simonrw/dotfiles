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

    enableFishIntegration = mkEnableOption "fish completion";
  };

  config = mkIf cfg.enable {
    home.packages = [
      cfg.package
    ];

    xdg.configFile."fish/completions/aws.fish".text = ''
      function __fish_complete_aws
          env COMP_LINE=(commandline -pc) aws_completer | tr -d ' '
      end

      complete -c aws -f -a "(__fish_complete_aws)"
    '';
  };
}
