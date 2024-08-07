{...}: {
  homebrew = {
    enable = true;
    # remove any homewbrew packages not installed via nix-darwin
    onActivation.cleanup = "uninstall";
    casks = [
      "1password"
      "alacritty"
      "arc"
      "barrier"
      "brave-browser"
      "dash"
      "docker"
      "element"
      "firefox"
      "gimp"
      "google-chrome"
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
      "visual-studio-code"
      "vlc"
      "whatsapp"
      "wireshark"
      "xquartz"
      "yubico-yubikey-manager"
    ];
    brews = [
      "emacs-plus@29"
    ];
    taps = [
      "homebrew/cask-versions"
      "d12frosted/emacs-plus"
    ];
    masApps = {
      DaisyDisk = 411643860;
      Tailscale = 1475387142;
    };
  };
}
