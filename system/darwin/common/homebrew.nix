{...}: {
  homebrew = {
    enable = true;
    # remove any homewbrew packages not installed via nix-darwin
    onActivation.cleanup = "uninstall";
    casks = [
      "1password"
      "1password-cli"
      "alacritty"
      "arc"
      "barrier"
      "brave-browser"
      "chatgpt"
      "dash"
      "docker"
      "element"
      "firefox"
      "gimp"
      "hammerspoon"
      "hot"
      "inkscape"
      "intellij-idea-ce"
      "karabiner-elements"
      "kitty"
      "notion"
      "obs"
      "obsidian"
      "pocket-casts"
      "pycharm-ce"
      "raycast"
      "rectangle"
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
