{ pkgs, lib, isLinux, isDarwin, system, ... }:
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

  custom-vscode = pkgs.vscode;  # .fhsWithPackages (ps: with ps; [
    # rustup
    # zlib
    # openssl.dev
    # pkg-config
  # ]);

  fhs = (
    let base = pkgs.appimageTools.defaultFhsEnvArgs; in
    pkgs.buildFHSUserEnv (base // {
      name = "fhs";
      targetPkgs = pkgs: (base.targetPkgs pkgs) ++ [ pkgs.pkg-config ];
      profile = "export FHS=1";
      runScript = "fish";
      extraOutputsToInstall = [ "dev" ];
    })
  );
in
{
  home.packages = with pkgs; [
    ansi
    awscli2
    bat
    cachix
    cert-info
    comma
    curlie
    custom-curl
    deadnix
    dig
    du-dust
    entr
    fd
    file
    fx
    gcc
    git-branchless
    graphviz
    hey
    htop
    httpie
    hub
    jujutsu
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
    sqlite-interactive
    universal-ctags
    unzip
    watch
    wget
    zip
    zsh
  ] ++ [
    # local packages
    listprojects
    notify-wrapper
  ] ++ (lib.optionals isDarwin [
    reattach-to-user-namespace
    coreutils
    libiconv
  ]) ++ (lib.optionals isLinux [
    blanket
    custom-vscode
    element-desktop
    fhs
    flameshot
    freetube
    gimp
    groff
    keymapp
    libreoffice
    obsidian
    pinentry-gtk2
    playerctl
    virt-manager
    vlc
    zeal
  ]) ++ (lib.optionals (system == "x86_64-linux") [
    slack
    discord
    insomnia
    lorien
    obs-studio
    jetbrains.pycharm-community
    shotcut
    wally
    wally-cli
    zoom-us
  ]);
}
