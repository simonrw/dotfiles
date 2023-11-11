{ config, pkgs, isLinux, isDarwin, lib, system, ... }:
let
  homeDir = if isDarwin then "Users" else "home";
  homeDirectory = "/${homeDir}/simon";
  browser = {
    name = "firefox";
    command = "${pkgs.firefox-devedition}/bin/firefox";
  };
in
{
  imports = [
    ./alacritty.nix
    ./bat.nix
    ./colours.nix
    ./dark-mode.nix
    ./default-applications.nix
    ./direnv.nix
    ./eza.nix
    ./emacs.nix
    ./fish.nix
    ./font.nix
    ./fzf.nix
    ./gh.nix
    ./git.nix
    ./gpg.nix
    ./helix.nix
    ./home-manager.nix
    ./ipython.nix
    ./jq.nix
    ./kitty.nix
    ./modules/contour.nix
    ./mpv.nix
    ./neovim.nix
    ./nix-index.nix
    ./packages/simon.nix
    ./ssh.nix
    ./tmux.nix
    ./wireshark.nix
    ./zoxide.nix
  ] ++ lib.optionals isLinux [
    ./rofi.nix
    ./chromium.nix
    ./kde.nix
    # ./xfce.nix
    ./gnome.nix
    ./i3.nix
    ./gtk.nix
    # ./sway.nix
    # ./mate.nix
    ./cinnamon.nix
    ./pantheon.nix
    ./xcape.nix
    ./bspwm.nix
    ./mousetracker.nix
  ] ++ lib.optionals isDarwin [
  ];

  home = {
    inherit homeDirectory;
    username = "simon";
    stateVersion = "22.05";

    sessionVariables = {
      # XXX work around https://github.com/NixOS/nixpkgs/issues/32580
      WEBKIT_DISABLE_COMPOSITING_MODE = 1;
    };
  };

  # custom properties
  me = {
    font-name = "Monaspace";
    font-style = "Semibold";
    font-size = if isLinux then 10.0 else 12.0;
    theme = "nord";
    vscode-theme = "Dracula";
    defaults = {
      inherit browser;
      # some dodgy font rendering on linux
      terminal = if isLinux then "kitty" else "alacritty";
    };
  } // (if isLinux then {
    wm.cinnamon = {
      enable = system != "aarch64-linux";
      dark-mode = true;
    };
  } else { });

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

    ".cargo/config.toml" = {
      text = ''
        [registries.crates-io]
        protocol = "sparse"
      '';
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
