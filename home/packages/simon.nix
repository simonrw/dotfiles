{ pkgs, lib, isLinux, isDarwin, ... }:
let
  fonts = with pkgs; [
    source-code-pro
    fira-code
    jetbrains-mono
    iosevka
    ibm-plex
    input-fonts
  ];
in
{
  home.packages = with pkgs; [
    _1password
    awscli2
    bat
    cachix
    cargo
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
    ncdu
    nixpkgs-fmt
    nix-tree
    nodejs
    noti
    nurl
    openssh
    pre-commit
    pyright
    python3
    python3.pkgs.pipx
    python3.pkgs.send2trash
    python3.pkgs.virtualenv
    ripgrep
    rnix-lsp
    rust-analyzer
    rustc
    tree-grepper
    universal-ctags
    unzip
    vscode
    zip
    zsh
  ] ++ fonts ++ [
    # local packages
    cftail
    listprojects
    snslistener
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
    jetbrains.pycharm-community
    notion
    obsidian
    pinentry-gtk2
    playerctl
    rofi
    slack
    steam
    telegram-desktop
    virt-manager
    zeal
  ]);
}
