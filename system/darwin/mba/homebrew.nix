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
      "freecad"
      "freetube"
      "gimp"
      "hammerspoon"
      "inkscape"
      "karabiner-elements"
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
      "wireshark"
      "xquartz"
    ];
    taps = [
      "homebrew/cask-versions"
    ];
    masApps = {
      DaisyDisk = 411643860;
      "GoodNotes 5" = 1444383602;
      Tailscale = 1475387142;
      "Bear – Markdown Notes" = 1091189122;
      "iA Writer" = 775737590;
      "Microsoft Remote Desktop" = 1295203466;
    };
  };
}
