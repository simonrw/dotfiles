{
  config,
  lib,
  pkgs,
  isLinux,
  ...
}:
with lib; let
  cfg = config.me;
in {
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
        "Noto Sans Mono"
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
    me.fonts-to-install = mkOption {
      type = types.listOf types.package;
      description = "Fonts to install regardless of what is chosen";
      default = [];
    };
  };

  config = let
    nerdfont-name =
      {
        "JetBrains Mono" = "JetBrainsMono";
        "Source Code Pro" = "SourceCodePro";
        "Inconsolata" = "Inconsolata";
      }
      .${cfg.font-name}
      or null;
    # TODO: hard-code comic mono here
    font-package =
      if nerdfont-name != null
      then (pkgs.nerdfonts.override {fonts = [nerdfont-name];})
      else
        {
          "Monaspace" = pkgs.monaspace;
          "Noto Sans Mono" = pkgs.noto-fonts;
        }
        .${cfg.font-name};

    alacritty-font-renamed =
      {
        "IBM Plex" = "IBM Plex Mono";
        "Inconsolata" = "Inconsolata Nerd Font Mono";
        "JetBrains Mono" = "JetBrainsMono Nerd Font Mono";
        "Monaspace" = "Monaspace Neon Var";
      }
      .${cfg.font-name}
      or cfg.font-name;

    kitty-font =
      {
        "JetBrains Mono" = "JetBrainsMono NF Bold";
        "Fira Code" = "Fira Code SemiBold";
        "Monaspace" = "Monaspace Neon Var SemiBold";
      }
      .${cfg.font-name}
      or cfg.font-name;

    alacritty-font-style-renamed =
      {
        Semibold =
          if isLinux
          then "ExtraBold"
          else "Bold";
      }
      .${cfg.font-style}
      or cfg.font-style;

    vscode-font =
      {
        "JetBrains Mono" = "JetBrains Mono Semibold";
        "Source Code Pro" = "Source Code Pro Semibold";
      }
      .${cfg.font-name};

    # bump up the font a bit
    rofi-font = "${cfg.font-name} ${
      builtins.elemAt (lib.strings.splitString "."
        (toString (cfg.font-size + 2.0)))
      0
    }";
  in {
    # vs code font
    programs.vscode.userSettings."editor.fontFamily" = vscode-font;
    programs.alacritty.settings.font = {
      normal.family = alacritty-font-renamed;
      normal.style = alacritty-font-style-renamed;
    };
    programs.alacritty.settings.font.size = cfg.font-size;
    programs.kitty.settings = {
      font_family = kitty-font;
      font_size = builtins.toString cfg.font-size;
    };

    programs.rofi.font = rofi-font;

    home.packages =
      [
        font-package
      ]
      ++ cfg.fonts-to-install;

    xsession.windowManager.i3.config = {
      fonts = {
        names = [cfg.font-name];
        style = cfg.font-style;
        size = cfg.font-size;
      };
    };
  };
}
