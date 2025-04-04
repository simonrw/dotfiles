{
  isDarwin,
  lib,
  ...
}: let
  mod-key =
    if isDarwin
    then "Command"
    else "Alt";

  option-as-alt-block =
    if isDarwin
    then {
      option_as_alt = "Both";
    }
    else {};
in {
  programs.alacritty = {
    enable = true;
    settings = {
      env = {
        "TERM" = "xterm-256color";
      };
      window =
        {
          dimensions = {
            columns = 120;
            lines = 40;
          };
        }
        // option-as-alt-block;
      keyboard = {
        bindings =
          [
            {
              key = "F";
              mods = "Alt";
              chars = "\\u001bf";
            }
            {
              key = "B";
              mods = "Alt";
              chars = "\\u001bb";
            }
            {
              key = "D";
              mods = "Alt";
              chars = "\\u001bd";
            }
            {
              key = "N";
              mods = mod-key;
              action = "SpawnNewInstance";
            }
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
