{pkgs, ...}: {
  programs.emacs = {
    enable = false;
    package = pkgs.emacs29;
    extraPackages = epkgs: [
      epkgs.magit
      epkgs.vterm
      epkgs.nix-mode
      epkgs.rust-mode
      epkgs.which-key
      epkgs.company
      epkgs.projectile
      epkgs.ripgrep
      epkgs.blacken
      epkgs.direnv
      epkgs.rustic
      epkgs.nord-theme
      epkgs.evil
    ];
    extraConfig = builtins.readFile ./emacs/init.el;
  };
}
