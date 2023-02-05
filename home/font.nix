{ config, lib, ... }:
with lib;
let
  font-name = config.editor-font;
in
{
  options = {
    editor-font = mkOption {
      type = types.enum [
        "Fira Code"
        "Hack"
        "IBM Plex"
        "JetBrains Mono"
        "Source Code Pro"
      ];
      description = ''
        Which font to use
      '';
    };
  };

  config =
    {
      # vs code font
      programs.vscode.userSettings."editor.fontFamily" = font-name;
    };
}

