{ pkgs, isLinux, ... }:
let
  base-package = if isLinux then pkgs.emacs-gtk else pkgs.emacs;

  emacs-package = (pkgs.emacsPackagesFor base-package).emacsWithPackages (epkgs: [
    epkgs.magit
    epkgs.vterm
    epkgs.nix-mode
    epkgs.rust-mode
    epkgs.which-key
    epkgs.company
    epkgs.projectile
    epkgs.ripgrep
    epkgs.eglot
    epkgs.blacken
    epkgs.direnv
  ]);
  extraConfig = ''
    (blink-cursor-mode 0)
    (setq inhibit-splash-screen t)
    (setq inhibit-startup-message t)
    (dolist (m '(tooltip-mode tool-bar-mode scroll-bar-mode menu-bar-mode))
      (when (fboundp m) (funcall m -1)))
    (show-paren-mode 1)
    (prefer-coding-system 'utf-8)
    (fset 'yes-or-no-p 'y-or-n-p)
    (set-language-environment "UTF-8")
    (set-buffer-file-coding-system 'utf-8)
    (set-default 'truncate-lines nil)
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
    (setq mouse-wheel-progressive-speed nil)
    (setq mouse-wheel-follow-mouse t)
    (add-hook 'before-save-hook 'delete-trailing-whitespace)
    (global-visual-line-mode t)
    (set-fringe-mode 10)

    ;; Enable nicer window moving
    (when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))

    ;; Disable visual bell
    (setq visible-bell nil)
    (setq ring-bell-function 'ignore)

    (setq indent-tabs-mode nil)
    (setq-default tab-width 4)

    (set-face-attribute 'default nil :family "Input Mono" :height 120)
    (set-face-attribute 'fixed-pitch nil :family "Input Mono" :height 120)
    ;; (set-face-attribute 'variable-pitch nil :family "Cantarell" :height 120)
    (set-face-attribute 'default (selected-frame) :height 120)

    ;; Configure company
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.0)

    ;; Enable eglot
    (add-hook 'python-mode-hook 'eglot-ensure)
    (add-hook 'rust-mode-hook 'eglot-ensure)

    ;; blacken
    (add-hook 'python-mode-hook 'blacken-mode)
  '';
in
{
  programs.emacs = {
    inherit extraConfig;
    enable = true;
    package = emacs-package;
  };
} // (if isLinux then
  {
    services.emacs = {
      enable = true;
      package = emacs-package;
    };
  } else { })
