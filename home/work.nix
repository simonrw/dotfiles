{ config, pkgs, isLinux, isDarwin, lib, ... }:
let
  homeDirectory = "/home/work";
in
{
  imports = [
    ./bat.nix
    ./colours.nix
    ./dark-mode.nix
    ./direnv.nix
    ./emacs.nix
    ./fish.nix
    ./font.nix
    ./fzf.nix
    ./gh.nix
    ./git.nix
    ./gnome.nix
    ./gpg.nix
    ./helix.nix
    ./home-manager.nix
    ./jq.nix
    ./neovim.nix
    ./nix-index.nix
    ./packages/work.nix
    ./tmux.nix
    # ./vscode.nix
  ];

  home = {
    inherit homeDirectory;
    username = "work";
    stateVersion = "22.05";
  };

  me = {
    dark-mode = true;
    font-name = "JetBrains Mono";
    font-size = 12.0;
    theme = "srw";
  };

  home.file = {
    ".bin" = {
      source = ./bin;
      recursive = true;
    };

    ".profile" = {
      source = pkgs.writeText ".profile" ''
        export XDG_DATA_DIRS=$HOME/.nix-profile/share/applications:$XDG_DATA_DIRS
      '';
    };

    ".npmrc" = {
      text = ''
        prefix = ${homeDirectory}/.npm-packages
      '';
    };

    ".hammerspoon" = lib.mkIf isDarwin {
      source = ./hammerspoon;
      recursive = true;
    };

    ".gnupg/gpg-agent.conf" = lib.mkIf isDarwin {
      text = ''
        default-cache-ttl 600
        max-cache-ttl 7200
        pinentry-program ${pkgs.pinentry_mac}/${pkgs.pinentry_mac.binaryPath}
      '';
    };

    ".ideavimrc" = {
      source = ./ideavimrc;
    };

    ".digrc" = {
      text = ''
        +noall +answer
      '';
    };
  };

  xdg = {
    enable = true;
    configFile.nvim = {
      source = ./nvim;
      recursive = true;
    };
    configFile."alacritty/alacritty.yml" =
      let
        theme =
          if config.me.dark-mode
          then "colors_default"
          else "colors_papercolor";

        originalText =
          builtins.readFile ./alacritty/alacritty.yml;

        replacedText =
          lib.replaceStrings [ "CHOSEN_COLOR_THEME" ] [ theme ] originalText;
      in
      {
        text = replacedText;
      };
    configFile.karabiner = lib.mkIf isDarwin {
      source = ./karabiner;
      recursive = true;
    };
  };

  # currently this is broken
  disabledModules = [ "targets/darwin/linkapps.nix" ];

  services.gpg-agent = lib.mkIf isLinux {
    enable = true;
    pinentryFlavor = "gtk2";
    enableFishIntegration = true;
    enableBashIntegration = true;
  };
}
