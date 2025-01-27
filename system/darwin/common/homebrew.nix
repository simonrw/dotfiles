{hostname, ...}: let
  host-specific-casks =
    {
      mba = [];
      mm = ["wacom-tablet"];
    }
    .${hostname};
in {
  homebrew = {
    enable = true;
    # remove any homewbrew packages not installed via nix-darwin
    onActivation.cleanup = "uninstall";
    casks =
      [
        "1password"
        "1password-cli"
        "alacritty"
        "barrier"
        "blender"
        "brave-browser"
        "deskpad"
        "discord"
        "dteoh-devdocs"
        "element"
        "emacs"
        "firefox"
        "ghostty"
        "gimp"
        "inkscape"
        "karabiner-elements"
        "notion"
        "obs"
        "obsidian"
        "pocket-casts"
        "pycharm-ce"
        "raycast"
        "shotcut"
        "slack"
        "steam"
        "sublime-merge"
        "sublime-text"
        "tabby"
        "todoist"
        "utm"
        "visual-studio-code"
        "vlc"
        "vmware-fusion"
        "whatsapp"
        "wireshark"
        "xquartz"
        "yt-music"
        "yubico-yubikey-manager"
        "zed"
      ]
      ++ host-specific-casks;
    brews = [
      "cargo-instruments"
      "cftail"
      # TODO: use nix-darwin module when ready
      # https://github.com/LnL7/nix-darwin/pull/1275
      {
        name = "colima";
        restart_service = "changed";
      }
      "docker"
      "node"
      "pulumi/tap/pulumi"
    ];
    taps = [
      "pulumi/tap"
      "simonrw/cftail"
    ];
    masApps = {
      DaisyDisk = 411643860;
      Tailscale = 1475387142;
      "AdGuard for Safari" = 1440147259;
      "1Password for Safari" = 1569813296;
      Instapaper = 288545208;
      Perplexity = 6714467650;
    };
  };
}
