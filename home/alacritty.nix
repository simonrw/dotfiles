{
  config,
  isDarwin,
  lib,
  ...
}:
with lib; let
  cfg = config.me.alacritty;
in {
  options = {
    me.alacritty.mod-key = mkOption {
      type = types.str;
      default =
        if isDarwin
        then "Command"
        else "Alt";
      description = "Default modifier key to use for alacritty";
    };
  };
  config = {
    programs.alacritty = {
      enable = true;
      settings = {
        env = {
          "TERM" = "xterm-256color";
        };
        window.dimensions = {
          columns = 120;
          lines = 40;
        };
        option_as_alt =
          if isDarwin
          then "Both"
          else "None";
        "key_bindings" =
          [
            {
              key = "F";
              mods = "Alt";
              chars = "\\x1bf";
            }
            {
              key = "B";
              mods = "Alt";
              chars = "\\x1bb";
            }
            {
              key = "D";
              mods = "Alt";
              chars = "\\x1bd";
            }
            {
              key = "N";
              mods = cfg.mod-key;
              action = "SpawnNewInstance";
            }
            # for some reason this is not picked up
          ]
          ++ lib.optionals isDarwin [
            # disable backwards search
            {
              key = "B";
              mods = "Command";
              action = "ReceiveChar";
            }
          ];
      };
    };
  };
}
