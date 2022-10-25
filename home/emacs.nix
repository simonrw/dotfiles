{ pkgs ? import <nixpkgs> { } }:
{
  enable = true;
  extraPackages = epkgs: with epkgs; [
    magit
    exec-path-from-shell
    markdown-mode
    rust-mode
    nix-mode
    nixpkgs-fmt
    cargo
    toml-mode
    cmake-mode
    python-pytest
    go-mode
    dockerfile-mode
    yaml-mode
    which-key
    speed-type
    vterm
    company
    projectile
    ripgrep
    fzf
    eglot
  ];
  extraConfig = (builtins.readFile ./emacs/init.el) + ''
    (setq explicit-shell-file-name "${pkgs.fish}/bin/fish")
    (setq shell-file-name "fish")
    (setenv "SHELL" shell-file-name)
  '';
}
