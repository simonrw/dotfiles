{ lib, config, ... }:
with lib;
let
  cfg = config.me.nixvim.which-key;
in
  {
    options.me.nixvim.which-key.enable = mkEnableOption "which-key";

    config = mkIf cfg.enable {
      programs.nixvim.plugins.which-key = {
        enable = true;
      };
    };
  }
