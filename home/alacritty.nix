{
  config,
  isDarwin,
  lib,
  ...
}:
with lib; let
  mod-key =
    if isDarwin
    then "Command"
    else "Alt";
in {
  programs.alacritty = {
    enable = true;
    settings = {
      env = {
        "TERM" = "xterm-256color";
      };
      window = {
        dimensions =
          {
            columns = 120;
            lines = 40;
          }
          // (
            if isDarwin
            then {
              option_as_alt =
                if isDarwin
                then "Both"
                else "None";
            }
            else {}
          );
      };
      keyboard = {
        bindings =
          [
            {
              key = "F";
              mods = "Alt";
              chars = "\x1bf";
            }
            {
              key = "B";
              mods = "Alt";
              chars = "\x1bb";
            }
            {
              key = "D";
              mods = "Alt";
              chars = "\x1bd";
            }
            {
              key = "N";
              mods = mod-key;
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
