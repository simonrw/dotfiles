{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.wezterm;
in {
  options = {
    me.wezterm.enable = mkEnableOption "wezterm configuration";
  };
  config.programs.wezterm = mkIf cfg.enable {
    enable = true;
    extraConfig = builtins.readFile ./wezterm.lua;
  };
}
