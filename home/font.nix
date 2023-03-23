{ config, lib, pkgs, isLinux, ... }:
with lib;
let
  cfg = config.me;

  # bump up the font size on linux since the font rendering is not so nice
  font-style = if isLinux then "Bold" else "Regular";
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
        normal.style = font-style;
        italic.family = cfg.font-name;
        italic.style = font-style;
      };
      programs.alacritty.settings.font.size = cfg.font-size;

      home.packages = [
        font-package
      ];

    };
}

