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
      "discord"
      "docker"
      "element"
      "emacs"
      "firefox"
      "gimp"
      "inkscape"
      "karabiner-elements"
      "obs"
      "obsidian"
      "pocket-casts"
      "pycharm-ce"
      "raycast"
      "shotcut"
      "slack"
      "sublime-merge"
      "sublime-text"
      "todoist"
      "utm"
      "visual-studio-code"
      "vlc"
      "vmware-fusion"
      "whatsapp"
      "wireshark"
      "xquartz"
      "yubico-yubikey-manager"
      "zed"
    ];
    brews = [
      "node"
      "cftail"
    ];
    taps = [
      "simonrw/cftail"
    ];
    masApps = {
      DaisyDisk = 411643860;
      Tailscale = 1475387142;
    };
  };
}
