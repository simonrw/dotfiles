{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.notifications;
in {
  options.me.nixvim.notifications = {
    enable = mkEnableOption "Notifications";
  };

  config = mkIf cfg.enable {
    plugins.fidget = {
      enable = true;
      notification.window = {
        align = "top";
      };
    };
  };
}
