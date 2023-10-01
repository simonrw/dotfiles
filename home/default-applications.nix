{ lib, config, ... }:
with lib;
let
  cfg = config.me.defaults;

  browser-module = types.submodule {
    name = mkOption {
      type = str;
      description = "Name of the browser";
      required = true;
    };

    command = mkOption {
      type = str;
      description = "Command to run";
      required = true;
    };
  };
in
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

  config = {
    xsession.windowManager.i3.config.terminal = cfg.terminal;
  };
}
