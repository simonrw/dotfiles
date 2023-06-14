{ config, isDarwin, lib, ... }:
let
  mod-key = if isDarwin then "Command" else "Alt";
in
{
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
        option_as_alt = if isDarwin then "Both" else "None";
        "key_bindings" = [
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
            mods = mod-key;
            action = "SpawnNewInstance";
          }
        ];
      };
    };
  };
}
