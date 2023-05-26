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
    ansi
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
    git-branchless
    go
    graphviz
    hey
    htop
    httpie
    hub
    lsof
    mkcert
    multitail
    nix-output-monitor
    nixpkgs-fmt
    nix-tree
    nodejs
    noti
    openssh
    pre-commit
    pstree
    pv
    pyright
    python3
    python3.pkgs.pipx
    python3.pkgs.send2trash
    python3.pkgs.virtualenv
    ripgrep
    rustup
    sqlite-interactive
    universal-ctags
    unzip
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
    reattach-to-user-namespace
    coreutils
    libiconv
  ]) ++ (lib.optionals isLinux [
    _1password-gui
    element-desktop
    freetube
    gimp
    insomnia
    jetbrains.pycharm-community
    lorien
    notion
    wally-cli
    obsidian
    obs-studio
    pinentry-gtk2
    playerctl
    shotcut
    slack
    steam
    telegram-desktop
    virt-manager
    vlc
    zeal
    zoom-us
  ]);
}
