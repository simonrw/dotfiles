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
        "Monaspace"
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
      font-package = if nerdfont-name != null then (pkgs.nerdfonts.override { fonts = [ nerdfont-name ]; }) else {
        "Monaspace" = pkgs.monaspace;
      }.${cfg.font-name};

      alacritty-font-renamed = {
        "IBM Plex" = "IBM Plex Mono";
        "Inconsolata" = "Inconsolata Nerd Font Mono";
        "JetBrains Mono" = "JetBrainsMono Nerd Font Mono";
        "Monaspace" = "Monaspace Neon Var";
      }.${cfg.font-name} or cfg.font-name;

      kitty-font = {
        "JetBrains Mono" = "JetBrainsMono NF Bold";
        "Fira Code" = "Fira Code SemiBold";
      }.${cfg.font-name} or cfg.font-name;

      alacritty-font-style-renamed = {
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
        font_family = kitty-font;
        font_size = builtins.toString cfg.font-size;
      };
      programs.contour.settings.profiles.main.font = {
        size = cfg.font-size;
        regular = {
          family = alacritty-font-renamed;
          weight = "bold";
        };
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

