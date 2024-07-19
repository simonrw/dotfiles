{ lib, config, ... }:
with lib;
let
  cfg = config.me.nixvim.hardtime;
in
{
  options.me.nixvim.hardtime = {
    enable = mkEnableOption "hardtime";
  };

  config = mkIf cfg.enable {
    programs.nixvim = {
    plugins.hardtime = {
      enable = true;
      allowDifferentKey = true;
      disableMouse = false;
    };
  };
  };
}
