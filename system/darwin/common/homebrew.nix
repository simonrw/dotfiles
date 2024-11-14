{...}: {
  homebrew = {
    enable = true;
    # remove any homewbrew packages not installed via nix-darwin
    onActivation.cleanup = "uninstall";
    casks = [
      "1password"
      "1password-cli"
      "alacritty"
      "barrier"
      "brave-browser"
      "chatgpt"
      "dash"
      "deskpad"
      "discord"
      "docker"
      "element"
      "emacs"
      "firefox"
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
      "wireshark"
      "xquartz"
      "yubico-yubikey-manager"
      "zed"
    ];
    brews = [
      "cargo-instruments"
      "cftail"
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
    };
  };
}
