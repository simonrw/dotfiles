{ config, lib, ... }:
with lib;
{
  options = {
    simonrw.colours = {
      foreground = mkOption {
        type = types.str;
      };
      background = mkOption {
        type = types.str;
      };
    };
  };
  config = {
    simonrw.colours =
      if config.dark-mode then {
        foreground = "#edf0f2";
        background = "#24292e";
      } else { };
  };
}
