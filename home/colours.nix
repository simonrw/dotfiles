{ config, lib, ... }:
with lib;
{
  options = {
    me.colours = {
      foreground = mkOption {
        type = types.str;
      };
      background = mkOption {
        type = types.str;
      };
    };
  };
  config = {
    me.colours =
      if config.me.dark-mode then {
        foreground = "#edf0f2";
        background = "#24292e";
      } else { };
  };
}
