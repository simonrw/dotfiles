
;; Packages
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar srw/my-packages
  '(better-defaults
	rust-mode
	toml-mode
	molokai-theme))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      srw/my-packages)

;; Settings
(setq inhibit-startup-message t)
(setq visible-bell nil)

(define-key global-map (kbd "RET") 'newline-and-indent)
(global-visual-line-mode t)

;; Disable blinking cursor
(blink-cursor-mode 0)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Source Code Pro")
  (set-face-attribute 'default nil :height 140)

  ;; Add homebrew installed packages to load-path
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))

  ;; Toggle fullscreen mode
  (global-set-key [s-return] 'toggle-frame-fullscreen))

;; Language specifics
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)

;; Make sure everything is in utf-8
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8)

;; Open files in an existing frame instead of a new frame each time
(setq ns-pop-up-frames nil)

;; Compile bindings
(global-set-key (kbd "C-c C-k") 'compile)

;; Make sure line numbers are not on
(global-linum-mode 0)

;; Stop any bells
(setq ring-bell-function 'ignore)

;; Mouse control (BAD!)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; Theming
(load-theme 'molokai t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (toml-mode rust-mode molokai-theme better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
