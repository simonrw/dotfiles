(blink-cursor-mode 0)

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

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
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode -1)

;; Enable nicer window moving
(when (fboundp 'windmove-default-keybindings)
(windmove-default-keybindings))

;; Disable visual bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq indent-tabs-mode nil)
(setq-default tab-width 4)

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; evil of course
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Ido
(ido-mode 1)
(ido-everywhere 1)
(use-package smex
  :config
  (smex-initialize)
  :bind ("M-x" . smex))

;; configure ediff
(setq ediff-split-window-function 'split-window-horizontally
	  ediff-window-setup-function 'ediff-setup-windows-plain)

(defun srw-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'srw-ediff-hook)

(add-to-list 'default-frame-alist `(font . "JetBrainsMono Nerd Font"))

;; org
(setq org-agenda-files '("~/org"))

(defun system-dark-mode-enabled-ip ()
  "Check if dark mode is currently enabled."
  (pcase system-type
    ('darwin
     ;; Have to use osascript here as defaults returns inconsistent results
     ;; - AppleInterfaceStyleSwitchesAutomatically == 1 ;; exists only if the theme is set to auto
     ;; - AppleInterfaceStyle == Dark ;; exists only if the theme is set to dark
     ;; How to determine if theme is light or dark when Automatic Theme switching is in place?
     ;; Luckily, osascript can provide that detail
     (if (string= (shell-command-to-string "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\"") "true") t))
    ('gnu/linux
     ;; prefer-dark and default are possible options
     (if (string= (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme") "\'prefer-dark\'\n") t))))

(if (system-dark-mode-enabled-ip)
    (progn
      (use-package catppuccin-theme)
      (setq catppuccin-flavor 'macchiato)
      (load-theme 'catppuccin :no-confirm)))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))
