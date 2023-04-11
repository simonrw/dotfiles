{ pkgs, lib, isLinux, isDarwin, ... }:
let
  # fonts = with pkgs; [
  #   source-code-pro
  #   fira-code
  #   jetbrains-mono
  #   ibm-plex
  #   inconsolata
  # ];
  custom-curl = pkgs.curl.override {
    c-aresSupport = true;
  };
in
{
  home.packages = with pkgs; [
    _1password
    awscli2
    bat
    cachix
    comma
    curlie
    custom-curl
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
    nixpkgs-fmt
    nix-tree
    nodejs
    noti
    openssh
    pre-commit
    pstree
    pyright
    python3
    python3.pkgs.pipx
    python3.pkgs.send2trash
    python3.pkgs.virtualenv
    ripgrep
    rustup
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
    listprojects
    notify-wrapper
  ] ++ (lib.optionals isDarwin [
    # macos only
    reattach-to-user-namespace
    coreutils
    libiconv
  ]) ++ (lib.optionals isLinux [
    # linux only
    _1password-gui
    element-desktop
    freetube
    gimp
    insomnia
    jetbrains.pycharm-community
    notion
    obsidian
    obs-studio
    pinentry-gtk2
    playerctl
    shotcut
    slack
    steam
    telegram-desktop
    virt-manager
    zeal
    zoom-us
  ]);
}
