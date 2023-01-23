{ config, pkgs, isLinux, isDarwin, lib, ... }:
let
  homeDir = if isDarwin then "Users" else "home";
  homeDirectory = "/${homeDir}/simon";
in
{
  imports = [
    ./alacritty.nix
    ./bat.nix
    ./dark-mode.nix
    ./direnv.nix
    ./emacs.nix
    ./exa.nix
    ./fish.nix
    ./font.nix
    ./fzf.nix
    ./gh.nix
    ./git.nix
    ./gpg.nix
    ./helix.nix
    ./home-manager.nix
    ./jq.nix
    ./kitty.nix
    ./mpv.nix
    ./neovim.nix
    ./nix-index.nix
    ./packages/simon.nix
    ./tmux.nix
  ] ++ lib.optionals isLinux [
    ./chromium.nix
    # ./kde.nix
    # ./xfce.nix
    ./gnome.nix
  ];

  home = {
    inherit homeDirectory;
    username = "simon";
    stateVersion = "22.05";
  };

  # custom properties
  dark-mode = true;
  editor-font = "JetBrains Mono";

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
  };

  xdg = {
    enable = true;
    configFile.nvim = {
      source = ./nvim;
      recursive = true;
    };
    configFile.karabiner = lib.mkIf isDarwin {
      source = ./karabiner;
      recursive = true;
    };
  };

  services.gpg-agent = lib.mkIf isLinux {
    enable = true;
    pinentryFlavor = "gtk2";
    enableFishIntegration = true;
    enableBashIntegration = true;
  };
}
