{ config, pkgs, lib, ... }:
let
  loadConfig =
    name: import ./${name}.nix { inherit pkgs; };

  appendConfig = attrs: name: attrs // {
    "${name}" = loadConfig name;
  };

  loadProgramConfigs =
    names: builtins.foldl' appendConfig { } names;

  homeDir = if pkgs.stdenv.isDarwin then "Users" else "home";
in
{
  home = {
    username = "simon";
    homeDirectory = "/${homeDir}/simon";
    stateVersion = "22.05";

    sessionVariables = {
      LANG = "en_GB.UTF-8";
      LC_CTYPE = "en_GB.UTF-8";
      LC_ALL = "en_GB.UTF-8";
      EDITOR = "nvim";
      PAGER = "bat";
      MANPAGER = "sh -c 'col -bx | ${pkgs.bat}/bin/bat -l man -p'";
    };

    packages = with pkgs; [
      bat
      curl
      entr
      exa
      fd
      fzf
      gh
      graphviz
      hey
      htop
      httpie
      hub
      mkcert
      multitail
      noti
      openssh
      pre-commit
      python3
      python3Packages.pipx
      ripgrep
      skim
      universal-ctags
    ] ++ (lib.optionals stdenv.isDarwin [
      # macos only
      reattach-to-user-namespace
    ]);
  };

  programs = loadProgramConfigs [
    "bat"
    "home-manager"
    "jq"
    "direnv"
    "fish"
    "git"
    "neovim"
    "tmux"
  ];

  home.file = {
    ".bin" = {
      source = ./bin;
      recursive = true;
    };

    ".hammerspoon" = lib.mkIf pkgs.stdenv.isDarwin {
      source = ./hammerspoon;
      recursive = true;
    };
  };

  xdg = {
    enable = true;
    configFile.nvim = {
      source = ./nvim;
      recursive = true;
    };
    configFile.alacritty = {
      source = ./alacritty;
      recursive = true;
    };
    configFile.karabiner = lib.mkIf pkgs.stdenv.isDarwin {
      source = ./karabiner;
      recursive = true;
    };
  };
}
