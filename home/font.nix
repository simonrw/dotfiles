{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.me;
in
{
  options = {
    me.font-name = mkOption {
      type = types.enum [
        "Fira Code"
        "Hack"
        "IBM Plex"
        "JetBrains Mono"
        "Source Code Pro"
        "Inconsolata"
        "MesloLGS NF"
      ];
      description = ''
        Which font to use
      '';
    };
    me.font-size = mkOption {
      type = types.float;
      description = ''
        Font size in px
      '';
    };
  };

  config =
    let
      font-package = {
        "Inconsolata" = pkgs.inconsolata;
        "JetBrains Mono" = pkgs.jetbrains-mono;
        "MesloLGS NF" = pkgs.meslo-lgs-nf;
      }.${cfg.font-name};
    in
    {
      # vs code font
      programs.vscode.userSettings."editor.fontFamily" = cfg.font-name;
      programs.alacritty.settings.font = {
        normal.family = cfg.font-name;
        normal.style = "Regular";
        italic.family = cfg.font-name;
        italic.style = "Regular";
      };
      programs.alacritty.settings.font.size = cfg.font-size;

      home.packages = [
        font-package
      ];

    };
}

