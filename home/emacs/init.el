;; Init file

;;; Defaults
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
(setq explicit-shell-file-name "/usr/local/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh.exe-args '("-i"))
(setenv "SHELL" shell-file-name)

;; Enable nicer window moving
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Disable visual bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq indent-tabs-mode nil)
(setq-default tab-width 4)

;;; OSX
(when (eq system-type 'darwin)

  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)

  (defun pbcopy ()
	(interactive)
	(call-process-region (point) (mark) "pbcopy")
	(setq deactivate-mark t))

  (defun pbpaste ()
	(interactive)
	(call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

  (defun pbcut ()
	(interactive)
	(pbcopy)
	(delete-region (region-beginning) (region-end)))

  (global-set-key (kbd "C-c c") 'pbcopy)
  (global-set-key (kbd "C-c v") 'pbpaste)
  (global-set-key (kbd "C-c x") 'pbcut)

  (menu-bar-mode t)

  (if (boundp 'mac-auto-operator-composition-mode)
	  (mac-auto-operator-composition-mode))

  ;; Toggle fullscreen mode
  (global-set-key [m-return] 'toggle-frame-fullscreen)

  (when (display-graphic-p)
	(setq-default mac-emulate-three-button-mouse t)
	(global-set-key (kbd "M-`") 'other-frame)))

(set-face-attribute 'default nil :family "Source Code Pro" :height 150)
(set-face-attribute 'fixed-pitch nil :family "Source Code Pro" :height 150)
(set-face-attribute 'variable-pitch nil :family "Cantarell" :height 150)
(set-face-attribute 'default (selected-frame) :height 150)

(load-theme 'wombat t)

(provide 'init)
