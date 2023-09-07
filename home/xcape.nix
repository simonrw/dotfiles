{ config, lib, ... }:
let
  cfg = config.me.xcape;
in
{
  options.me.xcape.enable = lib.mkEnableOption (lib.mkDoc "");

  config = {
    services.xcape = {
      enable = cfg.enable;
      mapExpression = {
        Control_L = "Escape";
      };
      timeout = 100;
    };
  };
}
