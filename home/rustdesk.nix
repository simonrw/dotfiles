{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.rustdesk;
in {
  options = {
    services.rustdesk = {
      enable = mkEnableOption "rustdesk";
      package = mkOption {
        type = types.package;
        default = pkgs.rustdesk;
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.rustdesk = {
      Unit = {
        Description = "Start rustdesk in server mode for remote desktop connections";
      };
      Service = {
        ExecStart = "${cfg.package}/bin/rustdesk --server";
      };
      Install = {
        WantedBy = ["multi-user.target"];
      };
    };
    home.packages = [
      cfg.package
    ];
  };
}
