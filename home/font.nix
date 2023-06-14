{ config, lib, pkgs, isLinux, ... }:
with lib;
let
  cfg = config.me;

  # bump up the font size on linux since the font rendering is not so nice
  font-style = if isLinux then "Semibold" else "Regular";
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
      nerdfont-name-map = {
        "JetBrains Mono" = "JetBrainsMono";
        "Source Code Pro" = "SourceCodePro";
      };
      font-package = pkgs.nerdfonts.override { fonts = [ nerdfont-name-map.${cfg.font-name} ]; };

      alacritty-font-renamed = {
        "IBM Plex" = "IBM Plex Mono";
      }.${cfg.font-name} or cfg.font-name;

      alacritty-font-style-renamed = {
        Semibold = "Bold";
      }.${font-style} or font-style;

      vscode-font = {
        "JetBrains Mono" = "JetBrains Mono Semibold";
      }.${cfg.font-name};
    in
    {
      # vs code font
      programs.vscode.userSettings."editor.fontFamily" = vscode-font;
      programs.alacritty.settings.font = {
        normal.family = alacritty-font-renamed;
        normal.style = alacritty-font-style-renamed;
        italic.family = alacritty-font-renamed;
        italic.style = alacritty-font-style-renamed;
      };
      programs.alacritty.settings.font.size = cfg.font-size;
      programs.kitty.settings = {
        font_family = "${cfg.font-name} ${font-style}";
        font_size = builtins.toString cfg.font-size;
      };

      home.packages = [
        font-package
      ];

    };
}

