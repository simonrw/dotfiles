{ pkgs, lib, isLinux, isDarwin, ... }:
let
  # fonts = with pkgs; [
  #   source-code-pro
  #   fira-code
  #   jetbrains-mono
  #   ibm-plex
  #   inconsolata
  # ];
in
{
  home.packages = with pkgs; [
    _1password
    awscli2
    bat
    cachix
    comma
    (curl.override {
      c-aresSupport = true;
    })
    curlie
    deadnix
    du-dust
    entr
    fd
    file
    fx
    gcc
    go
    graphviz
    hey
    htop
    httpie
    hub
    lsof
    mkcert
    multitail
    nix-tree
    nixpkgs-fmt
    nodejs
    noti
    openssh
    pre-commit
    pyright
    python3
    python3.pkgs.pipx
    python3.pkgs.send2trash
    python3.pkgs.virtualenv
    ripgrep
    tree-grepper
    universal-ctags
    unzip
    vscode
    watch
    wget
    zip
    zsh
  ] ++ [
    # local packages
    cargo-dist
    cftail
    listprojects
    notify-wrapper
    snslistener
  ] ++ (lib.optionals isDarwin [
    # macos only
    reattach-to-user-namespace
    coreutils
    libiconv
  ]) ++ (lib.optionals isLinux [
    # linux only
    _1password-gui
    element-desktop
    gimp
    insomnia
    jetbrains.pycharm-community
    notion
    obsidian
    pinentry-gtk2
    playerctl
    slack
    steam
    telegram-desktop
    virt-manager
    zeal
    zoom-us
  ]);
}
