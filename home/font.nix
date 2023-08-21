{ config, lib, pkgs, isLinux, ... }:
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
    me.font-style = mkOption {
      type = types.enum [
        "Regular"
        "Semibold"
        "Bold"
      ];
      description = "Style of font";
      default = "Regular";
    };
  };

  config =
    let
      nerdfont-name = {
        "JetBrains Mono" = "JetBrainsMono";
        "Source Code Pro" = "SourceCodePro";
        "Inconsolata" = "Inconsolata";
      }.${cfg.font-name} or null;
      # TODO: hard-code comic mono here
      font-package = if nerdfont-name != null then (pkgs.nerdfonts.override { fonts = [ nerdfont-name ]; }) else pkgs.comic-mono;

      alacritty-font-renamed = {
        "IBM Plex" = "IBM Plex Mono";
        "Inconsolata" = "Inconsolata Nerd Font Mono";
        "JetBrains Mono" = "JetBrains Mono NL";
      }.${cfg.font-name} or cfg.font-name;

      alacritty-font-style-renamed = {
        Semibold = "Bold";
      }.${cfg.font-style} or cfg.font-style;

      kitty-font-style-renamed = {
        Semibold = "Bold";
      }.${cfg.font-style} or cfg.font-style;

      vscode-font = {
        "JetBrains Mono" = "JetBrains Mono Semibold";
        "Source Code Pro" = "Source Code Pro Semibold";
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
        font_family = "${cfg.font-name} ${kitty-font-style-renamed}";
        font_size = builtins.toString cfg.font-size;
      };

      home.packages = [
        font-package
      ];

      xsession.windowManager.i3.config = {
        fonts = {
          names = [ cfg.font-name ];
          style = cfg.font-style;
          size = cfg.font-size;
        };
      };
    };
}

