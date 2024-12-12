{
  pkgs,
  hostname,
  config,
  ...
}: {
  imports = [
    ./fish-fix.nix
    ./homebrew.nix
    ./touch-id.nix
  ];

  users.users.simon = {
    name = "simon";
    home = "/Users/simon";
    shell = pkgs.fish;
  };

  nix = import ../../../common/nix-settings.nix {inherit pkgs;};

  fonts = {
    packages = with pkgs; [
      fantasque-sans-mono
      noto-fonts
      iosevka
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

  services.nix-daemon.enable = true;
  documentation.enable = true;

  services.tailscale.enable = true;

  # enable touch id for sudo
  # https://nixcademy.com/2024/01/15/nix-on-macos/
  security.pam.enableSudoTouchIdAuth = true;

  programs.fish = {
    enable = true;
    loginShellInit = ''
      eval (${config.homebrew.brewPrefix}/brew shellenv)
    '';
  };
  programs.zsh.enable = true;
  programs.gnupg.agent.enable = true;

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  # https://nixcademy.com/2024/01/15/nix-on-macos/
  system.defaults = {
    dock = {
      autohide = false;
      mru-spaces = false;
      orientation = "bottom";
      tilesize = {
        mm = 34;
        mba = 24;
      }.${hostname};
      largesize = 92;
      persistent-apps = [
        "/Applications/Brave Browser.app"
        "/Applications/Alacritty.app"
        "/Applications/Obsidian.app"
        "/Applications/Slack.app"
        "/Applications/Todoist.app"
      ];
    };
    finder = {
      AppleShowAllExtensions = true;
      # only search the current folder
      FXDefaultSearchScope = "SCcf";
      # don't warn about changing file extensions
      FXEnableExtensionChangeWarning = false;
      # show list view by default
      FXPreferredViewStyle = "Nlsv";
      # show breadcrumbs
      ShowPathbar = true;
      # show file path in title bar
      _FXShowPosixPathInTitle = true;
    };
    loginwindow.GuestEnabled = false;
    screencapture.location = "~/Pictures/screenshots";
    NSGlobalDomain = {
      AppleICUForce24HourTime = true;
      AppleInterfaceStyle = "Dark";
      KeyRepeat = 2;
      NSWindowShouldDragOnGesture = true;
    };
  };

  networking.hostName = hostname;

  # force the system to read the new settings
  system.activationScripts.postUserActivation.text = ''
    /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
  '';

  # configure system defaults
  system.defaults.".GlobalPreferences"."com.apple.mouse.scaling" = -1.0;

  system.stateVersion = 5;
}
