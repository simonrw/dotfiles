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
      alejandra
      ansi
      bat
      cachix
      cert-info
      curlie
      custom-curl
      dig
      docker-credential-helpers
      du-dust
      duckdb
      entr
      fd
      file
      go
      gron
      htop
      jless
      jqp
      lsof
      nix-output-monitor
      lua-language-server
      luaformatter
      mpv
      nil
      nix-tree
      openssh
      pre-commit
      pstree
      pv
      pyright
      python3
      ripgrep
      sqlite-interactive
      typescript-language-server
      universal-ctags
      unzip
      uv
      watch
      yq
      zip
    ]
    ++ [
      # local packages
      check-certificate-revocation
      codelldb
      listprojects
      localdocs
      logtimes
      notify-wrapper
      realise-symlink
      simpleproxy
      testsearch
      zizmor
    ]
    ++ (lib.optionals isDarwin [
      reattach-to-user-namespace
      coreutils
      libiconv
      blueutil
    ])
    ++ (lib.optionals isLinux [
      blanket # white noise generator
      custom-vscode
      drawio
      nodejs
      element-desktop
      emote # emoji picker
      flameshot # screenshot utility
      freetube
      gimp
      groff
      keymapp
      libreoffice
      man-pages
      man-pages-posix
      obsidian
      playerctl
      remmina
      thunderbird
      todoist-electron
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
      jetbrains.idea-community-bin
      shotcut
      wally
      wally-cli
      zoom-us
    ]);
}
