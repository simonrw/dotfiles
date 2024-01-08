{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.defaults;

  names = [
    "google-chrome"
    "brave"
    "firefox"
  ];

  browser-module = types.submodule {
    options = {
      name = mkOption {
        type = types.enum names;
        description = "Name of the browser";
      };

      command = mkOption {
        type = types.str;
        description = "Command to run";
      };
    };
  };
in {
  options.me.defaults = {
    browser = mkOption {
      type = types.oneOf [
        (types.enum names)
        browser-module
      ];
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
