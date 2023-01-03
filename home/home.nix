{ config, pkgs, lib, ... }:
let
  homeDir = if pkgs.stdenv.isDarwin then "Users" else "home";
in
{
  imports = [
    ./bat.nix
    ./dark-mode.nix
    ./direnv.nix
    ./fish.nix
    ./fzf.nix
    ./gh.nix
    ./git.nix
    ./gpg.nix
    ./helix.nix
    ./home-manager.nix
    ./jq.nix
    ./neovim.nix
    ./nix-index.nix
    ./tmux.nix
    ./vscode.nix
  ];

  home = {
    username = "simon";
    homeDirectory = "/${homeDir}/simon";
    stateVersion = "22.05";

    sessionVariables = {
      LANG = "en_GB.UTF-8";
      LC_CTYPE = "en_GB.UTF-8";
      LC_ALL = "en_GB.UTF-8";
      EDITOR = "${pkgs.neovim}/bin/nvim";
      PAGER = "${pkgs.bat}/bin/bat";
      MANPAGER = "sh -c 'col -bx | ${pkgs.bat}/bin/bat -l man -p'";
    };

    packages = with pkgs; [
      _1password
      awscli2
      bat
      cachix
      cargo
      comma
      curl
      curlie
      deadnix
      du-dust
      entr
      exa
      fd
      fx
      gcc
      go
      graphviz
      hey
      htop
      httpie
      hub
      input-fonts
      lsof
      mkcert
      multitail
      ncdu
      nixpkgs-fmt
      nodejs
      noti
      nurl
      openssh
      pre-commit
      pyright
      python3
      python3.pkgs.pipx
      python3.pkgs.send2trash
      python3.pkgs.virtualenv
      ripgrep
      rnix-lsp
      rust-analyzer
      rustc
      tree-grepper
      universal-ctags
      unzip
      zip
      zsh
    ] ++ [
      # local packages
      cftail
      listprojects
      ntfy
      snslistener
      notify-wrapper
    ] ++ (lib.optionals stdenv.isDarwin [
      # macos only
      reattach-to-user-namespace
      coreutils
      libiconv
    ]) ++ (lib.optionals stdenv.isLinux [
      # linux only
      _1password-gui
      element-desktop
      jetbrains.pycharm-community
      notion
      obsidian
      pinentry-gtk2
      rofi
      slack
      telegram-desktop
      zeal
    ]);
  };

  dark-mode = true;

  # TODO: disabled for now
  xsession.windowManager.i3 =
    let mod = "Mod1";
    in
    lib.mkIf false {
      enable = true;
      config = {
        modifier = mod;
        fonts = {
          names = [
            "Source Code Pro"
          ];
          style = "Bold Semi-Condensed";
          size = 11.0;
        };
        startup =
          let
            execAlways = cmd: {
              command = cmd;
              always = true;
              notification = false;
            };
          in
          [
            (execAlways ''hsetroot -solid "#c2dced"'')
            (execAlways ''bash ~/.bin/set-keyboard'')
          ];
        terminal = "${pkgs.alacritty}/bin/alacritty";
        menu = "${pkgs.rofi}/bin/rofi -show drun";
        window = {
          titlebar = false;
          border = 1;
        };
        bars = [
          {
            position = "top";
            fonts = {
              names = [ "Source Code Pro" ];
            };
          }
        ];
        keybindings = {
          "${mod}+Shift+Q" = "kill";
          # change focus
          "${mod}+h" = "focus left";
          "${mod}+j" = "focus down";
          "${mod}+k" = "focus up";
          "${mod}+l" = "focus right";
          # alternatively, you can use the cursor keys:
          "${mod}+Left" = "focus left";
          "${mod}+Down" = "focus down";
          "${mod}+Up" = "focus up";
          "${mod}+Right" = "focus right";

          # move focused window
          "${mod}+Shift+H" = "move left";
          "${mod}+Shift+J" = "move down";
          "${mod}+Shift+K" = "move up";
          "${mod}+Shift+L" = "move right";

          # alternatively, you can use the cursor keys:
          "${mod}+Shift+Left" = "move left";
          "${mod}+Shift+Down" = "move down";
          "${mod}+Shift+Up" = "move up";
          "${mod}+Shift+Right" = "move right";

          # split in horizontal orientation
          "${mod}+Shift+backslash" = "split h";

          # split in vertical orientation
          "${mod}+underscore" = "split v";

          # enter fullscreen mode for the focused container
          "${mod}+Shift+f" = "fullscreen";

          # change container layout (stacked, tabbed, default)
          "${mod}+s" = "layout stacking";
          "${mod}+w" = "layout tabbed";
          "${mod}+e" = "layout default";

          # toggle tiling / floating
          "${mod}+Shift+space" = "floating toggle";

          # change focus between tiling / floating windows
          "${mod}+space" = "focus mode_toggle";

          # focus the parent container
          "${mod}+a" = "focus parent";
          # switch to workspace
          "${mod}+1" = "workspace number 1";
          "${mod}+2" = "workspace number 2";
          "${mod}+3" = "workspace number 3";
          "${mod}+4" = "workspace number 4";
          "${mod}+5" = "workspace number 5";
          "${mod}+6" = "workspace number 6";
          "${mod}+7" = "workspace number 7";
          "${mod}+8" = "workspace number 8";
          "${mod}+9" = "workspace number 9";
          "${mod}+0" = "workspace number 10";
          "${mod}+n" = "workspace next";
          "${mod}+p" = "workspace prev";

          # move focused container to workspace
          "${mod}+Shift+exclam" = "move container to workspace 1";
          "${mod}+Shift+quotedbl" = "move container to workspace 2";
          "${mod}+Shift+sterling" = "move container to workspace 3";
          "${mod}+Shift+dollar" = "move container to workspace 4";
          "${mod}+Shift+percent" = "move container to workspace 5";
          "${mod}+Shift+asciicircum" = "move container to workspace 6";
          "${mod}+Shift+ampersand" = "move container to workspace 7";
          "${mod}+Shift+asterisk" = "move container to workspace 8";
          "${mod}+Shift+parenleft" = "move container to workspace 9";
          "${mod}+Shift+parenright" = "move container to workspace 10";

          # reload the configuration file
          "${mod}+Shift+C" = "reload";
          # restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
          "${mod}+Shift+R" = "restart";
          # exit i3 (logs you out of your X session)
          "${mod}+Ctrl+Shift+E" = "exit";

          "${mod}+c" = "exec ${pkgs.firefox}/bin/firefox";
          "${mod}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
          "${mod}+d" = "exec ${pkgs.rofi}/bin/rofi -show drun";
        };
      };
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

    ".hammerspoon" = lib.mkIf pkgs.stdenv.isDarwin {
      source = ./hammerspoon;
      recursive = true;
    };

    ".gnupg/gpg-agent.conf" = lib.mkIf pkgs.stdenv.isDarwin {
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
    configFile."alacritty/alacritty.yml" =
      let
        theme =
          if config.dark-mode
          then "colors_default"
          else "colors_papercolor";

        originalText =
          builtins.readFile ./alacritty/alacritty.yml;

        replacedText =
          lib.replaceStrings [ "CHOSEN_COLOR_THEME" ] [ theme ] originalText;
      in
      {
        # source = ./alacritty/alacritty.yml;
        text = replacedText;
      };
    configFile.karabiner = lib.mkIf pkgs.stdenv.isDarwin {
      source = ./karabiner;
      recursive = true;
    };
    configFile.kitty = {
      source = ./kitty;
      recursive = true;
    };
  };

  # currently this is broken
  disabledModules = [ "targets/darwin/linkapps.nix" ];

  services.gpg-agent = lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    pinentryFlavor = "gtk2";
    enableFishIntegration = true;
    enableBashIntegration = true;
  };
}
