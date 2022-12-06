{ pkgs, lib, ... }:
{
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    _1password
  ];

  users.users.simon = {
    name = "simon";
    home = "/Users/simon";
    shell = pkgs.fish;
  };

  nix.settings.auto-optimise-store = true;

  nix.gc = {
    automatic = true;
    interval = {
      Weekday = 0;
      Hour = 2;
    };
  };

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      fantasque-sans-mono
      jetbrains-mono
      fira-code
      source-code-pro
    ];
  };

  environment.shells = [
    pkgs.fish
  ];

  environment.variables.SHELL = "${pkgs.fish}/bin/fish";
  environment.loginShell = "${pkgs.fish}/bin/fish";

  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
  documentation.enable = true;
  nix.extraOptions = ''
    keep-outputs = false
    keep-derivations = true
    auto-optimise-store = true
    experimental-features = nix-command flakes
    trusted-users = simon root
  '' + lib.optionalString (pkgs.system == "aarch64-darwin") ''
    extra-platforms = x86_64-darwin aarch64-darwin
  '';

  programs.fish.enable = true;
  programs.zsh.enable = true;
  programs.nix-index.enable = true;
  programs.gnupg.agent.enable = true;

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  homebrew = {
    enable = true;
    casks = [
      "1password"
      "alacritty"
      "alfred"
      "barrier"
      "dash"
      "docker"
      "element"
      "firefox"
      "gimp"
      "google-chrome"
      "hammerspoon"
      "karabiner-elements"
      "kitty"
      "obsidian"
      "pycharm-ce"
      "vlc"
    ];
    masApps =
      {
        DaisyDisk = 411643860;
        "GoodNotes 5" = 1444383602;
        Tailscale = 1475387142;
        "Bear – Markdown Notes" = 1091189122;
        "iA Writer" = 775737590;
      };
  };
}
