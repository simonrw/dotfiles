{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    _1password
  ];

  users.users.simon = {
    name = "simon";
    home = "/Users/simon";
    shell = pkgs.fish;
  };

  nix = import ../../common/nix-settings.nix { inherit pkgs; };

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
  documentation.enable = true;

  programs.fish.enable = true;
  programs.zsh.enable = true;
  programs.nix-index.enable = true;
  programs.gnupg.agent.enable = true;

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  system.defaults = {
    dock.autohide = true;
  };

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
      "pocket-casts"
      "firefox"
      "gimp"
      "google-chrome"
      "hammerspoon"
      "karabiner-elements"
      "kitty"
      "notion"
      "obsidian"
      "pycharm-ce"
      "syncthing"
      "vlc"
    ];
    masApps =
      {
        DaisyDisk = 411643860;
        "GoodNotes 5" = 1444383602;
        Tailscale = 1475387142;
        "Bear – Markdown Notes" = 1091189122;
        "iA Writer" = 775737590;
        "Microsoft Remote Desktop" = 1295203466;
      };
  };
}
