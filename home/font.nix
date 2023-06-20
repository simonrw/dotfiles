{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.me;

  font-style = "Semibold";
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
        "Comic Mono"
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
      nerdfont-name = {
        "JetBrains Mono" = "JetBrainsMono";
        "Source Code Pro" = "SourceCodePro";
      }.${cfg.font-name} or null;
      # TODO: hard-code comic mono here
      font-package = if nerdfont-name != null then (pkgs.nerdfonts.override { fonts = [ nerdfont-name ]; }) else pkgs.comic-mono;

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

