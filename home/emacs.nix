{ pkgs ? import <nixpkgs> { } }:
{
  enable = true;
  extraPackages = epkgs: with epkgs; [
    magit
    exec-path-from-shell
    markdown-mode
    rust-mode
    nix-mode
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
  ];
  extraConfig = builtins.readFile ./emacs/init.el;
}
