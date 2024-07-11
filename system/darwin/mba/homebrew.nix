{...}: {
  homebrew = {
    enable = true;
    # remove any homewbrew packages not installed via nix-darwin
    onActivation.cleanup = "uninstall";
    casks = [
      "1password"
      "alacritty"
      "barrier"
      "brave-browser"
      "dash"
      "docker"
      "element"
      "firefox"
      "gimp"
      "hammerspoon"
      "hot"
      "inkscape"
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
      "sublime-merge"
      "sublime-text"
      "todoist"
      "visual-studio-code"
      "vlc"
      "whatsapp"
      "wireshark"
      "xquartz"
    ];
    taps = [
      "homebrew/cask-versions"
    ];
    masApps = {
      DaisyDisk = 411643860;
      Tailscale = 1475387142;
    };
  };
}
