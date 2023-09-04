{ lib, config, ... }:
let
  cfg = config.me.wm.sway;
in
  { 
    options.me.wm.sway.enable = lib.mkEnableOption (lib.mkDoc "Enable sway window manager");

    config = lib.mkIf cfg.enable {
      programs.sway.enable = true;
    };
  }
