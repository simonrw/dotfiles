{pkgs, ...}: {
  imports = [
    ./fish-fix.nix
  ];

  users.users.simon = {
    name = "simon";
    home = "/Users/simon";
    shell = pkgs.fish;
  };

  nix = import ../../../common/nix-settings.nix {inherit pkgs;};

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      fantasque-sans-mono
      (nerdfonts.override {
        fonts = [
          "JetBrainsMono"
          "SourceCodePro"
        ];
      })
      fira-code
      inconsolata
      monaspace
    ];
  };

  environment.shells = [
    pkgs.fish
  ];

  environment.variables.SHELL = "${pkgs.fish}/bin/fish";
  environment.loginShell = "${pkgs.fish}/bin/fish";

  services.nix-daemon.enable = true;
  documentation.enable = true;

  services.tailscale.enable = true;

  # enable touch id for sudo
  # https://nixcademy.com/2024/01/15/nix-on-macos/
  security.pam.enableSudoTouchIdAuth = true;

  programs.fish.enable = true;
  programs.zsh.enable = true;
  programs.nix-index.enable = true;
  programs.gnupg.agent.enable = true;

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  # https://nixcademy.com/2024/01/15/nix-on-macos/
  system.defaults = {
    dock = {
      autohide = true;
      mru-spaces = false;
      orientation = "bottom";
      tilesize = 48;
    };
    finder.AppleShowAllExtensions = true;
    screencapture.location = "~/Pictures/screenshots";
  };

  homebrew = {
    enable = true;
    casks = [
      "1password"
      "alacritty"
      "alfred"
      "barrier"
      "brave-browser"
      "dash"
      "contour"
      "docker"
      "element"
      "gimp"
      "google-chrome"
      "hammerspoon"
      "inkscape"
      "karabiner-elements"
      "mpv"
      "obsidian"
      "pocket-casts"
      "pycharm-ce"
      "shotcut"
      "sublime-merge"
      "sublime-text"
      "visual-studio-code"
      "vlc"
      "wireshark"
      "xquartz"
    ];
    taps = [
      "homebrew/cask-versions"
    ];
    masApps = {
      DaisyDisk = 411643860;
      "GoodNotes 5" = 1444383602;
      Tailscale = 1475387142;
      "Bear – Markdown Notes" = 1091189122;
      "iA Writer" = 775737590;
      "Microsoft Remote Desktop" = 1295203466;
    };
  };

  # force the system to read the new settings
  system.activationScripts.postUserActivation.text = ''
    /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
  '';

  # configure system defaults
  system.defaults.".GlobalPreferences"."com.apple.mouse.scaling" = -1.0;
}
