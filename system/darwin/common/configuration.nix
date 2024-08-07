{pkgs, ...}: {
  imports = [
    ./fish-fix.nix
    ./homebrew.nix
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
  programs.gnupg.agent.enable = true;

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  # https://nixcademy.com/2024/01/15/nix-on-macos/
  system.defaults = {
    dock = {
      autohide = true;
      mru-spaces = false;
      orientation = "bottom";
      tilesize = 24;
      largesize = 92;
    };
    finder.AppleShowAllExtensions = true;
    screencapture.location = "~/Pictures/screenshots";
  };

  # force the system to read the new settings
  system.activationScripts.postUserActivation.text = ''
    /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
  '';

  # configure system defaults
  system.defaults.".GlobalPreferences"."com.apple.mouse.scaling" = -1.0;
}
