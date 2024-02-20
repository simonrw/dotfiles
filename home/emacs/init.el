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

;; configure ediff
(setq ediff-split-window-function 'split-window-horizontally
	  ediff-window-setup-function 'ediff-setup-windows-plain)

(defun srw-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'srw-ediff-hook)

(set-face-attribute 'default nil :family "JetBrains Mono" :weight 'bold :height 120)
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :weight 'bold :height 120)
;; (set-face-attribute 'variable-pitch nil :family "Cantarell" :height 120)
(set-face-attribute 'default (selected-frame) :height 120)

;; org
(setq org-agenda-files '("~/org"))

;; show helpful hints when pressing keys
(which-key-mode t)

;; theme

(load-theme 'nord t)
