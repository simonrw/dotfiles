{
  pkgs,
  isLinux,
  isDarwin,
  lib,
  ...
}: let
  homeDir =
    if isDarwin
    then "Users"
    else "home";
  homeDirectory = "/${homeDir}/simon";
in {
  imports =
    [
      ./alacritty.nix
      ./atuin.nix
      ./aws.nix
      ./bat.nix
      ./chromium.nix
      ./colours.nix
      ./dark-mode.nix
      ./default-applications.nix
      ./direnv.nix
      ./emacs.nix
      ./eza.nix
      ./firefox.nix
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
      ./jujutsu.nix
      ./kitty.nix
      ./mpv.nix
      ./nix-index.nix
      ./nixvim
      ./packages/simon.nix
      ./zellij.nix
      ./ssh.nix
      ./taskwarrior.nix
      ./tmux.nix
      ./wireshark.nix
      ./zoxide.nix
      ./zsh.nix
    ]
    ++ lib.optionals isLinux [
      ./rofi
      ./kde.nix
      ./xfce.nix
      ./gnome.nix
      ./i3.nix
      ./gtk.nix
      ./sway.nix
      ./mate.nix
      ./cinnamon.nix
      ./pantheon.nix
      ./xcape.nix
      ./bspwm.nix
      ./hyprland.nix
      ./river.nix
    ]
    ++ lib.optionals isDarwin [
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
  me =
    {
      font-name = "JetBrains Mono";
      font-style = "Semibold";
      font-size =
        if isLinux
        then 10.0
        else 12.0;
      fonts-to-install = [
        pkgs.monaspace
      ];
      aws.enable = true;
      theme = "papercolor";
      vscode-theme = "Dracula";
      defaults = {
        browser = "brave";
        terminal = "alacritty";
      };
      is-dark-theme = false;
    }
    // (
      if isLinux
      then {
        wm.gnome = {
          enable = true;
          wayland = false;
        };
        wm.bspwm.enable = true;
        wm.cinnamon.enable = true;
      }
      else {}
    );

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
    configFile.karabiner = lib.mkIf isDarwin {
      source = ./karabiner;
      recursive = true;
    };
  };

  services.gpg-agent = lib.mkIf isLinux {
    enable = true;
    pinentryPackage = pkgs.pinentry-gnome3;
    enableFishIntegration = true;
    enableBashIntegration = true;
  };
}
