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
        "amazon-chime"
        "barrier"
        "blender"
        "brave-browser"
        "cursor"
        "deskpad"
        "discord"
        "docker"
        "dteoh-devdocs"
        "element"
        "emacs"
        "firefox"
        "ghostty"
        "gimp"
        "inkscape"
        "insomnia"
        "jetbrains-toolbox"
        "karabiner-elements"
        "neovide"
        "notion"
        "obs"
        "obsidian"
        "pocket-casts"
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
        "whatsapp"
        "wireshark"
        "xquartz"
        "yt-music"
        "yubico-yubikey-manager"
        "zed"
        "zoom"
      ]
      ++ host-specific-casks;
    brews = [
      "cargo-instruments"
      "node"
      "pulumi/tap/pulumi"
    ];
    taps = [
      "pulumi/tap"
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
