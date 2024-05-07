{
  pkgs,
  lib,
  isLinux,
  isDarwin,
  system,
  ...
}: let
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

  custom-vscode = pkgs.vscode;
  # custom-vscode = pkgs.vscode-with-extensions.override {
  #   vscodeExtensions = with pkgs.vscode-extensions; [
  #     vadimcn.vscode-lldb
  #   ];
  # };
in {
  home.packages = with pkgs;
    [
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
      docker-credential-helpers
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
      lnav
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
    ]
    ++ [
      # local packages
      listprojects
      notify-wrapper
      simpleproxy
    ]
    ++ (lib.optionals isDarwin [
      reattach-to-user-namespace
      coreutils
      libiconv
    ])
    ++ (lib.optionals isLinux [
      blanket
      custom-vscode
      drawio
      element-desktop
      emote
      flameshot
      freetube
      gimp
      groff
      keymapp
      libreoffice
      man-pages
      man-pages-posix
      obsidian
      playerctl
      thunderbird
      virt-manager
      vlc
      zeal
      zed-editor
    ])
    ++ (lib.optionals (system == "x86_64-linux") [
      slack
      discord
      insomnia
      lorien
      obs-studio
      jetbrains.pycharm-community-bin
      shotcut
      wally
      wally-cli
      zoom-us
    ]);
}
