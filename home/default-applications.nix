{ lib, ... }:
with lib;
{
  options.me.defaults = {
    browser = mkOption {
      type = types.oneOf [ types.str types.attrs ];
      description = "Which browser to use";
    };
    terminal = mkOption {
      type = types.str;
      description = "Which terminal emulator to use";
    };
  };
}
