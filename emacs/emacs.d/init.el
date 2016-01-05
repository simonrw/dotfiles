;; Packages
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar srw/my-packages
  '(better-defaults
    flycheck ;; syntax checker
    py-autopep8
    ein
	smex
    markdown-mode
    multi-term
	clojure-mode
	auto-complete
	cider
	paredit
	color-theme-solarized
	magit
    material-theme))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      srw/my-packages)

;; Settings
(setq inhibit-startup-message t)
(setq visible-bell nil)

(define-key global-map (kbd "RET") 'newline-and-indent)
(global-visual-line-mode t)

;; Hooks
(add-hook 'markdown-mode-hook
		  (auto-fill-mode))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Hack")
  (set-face-attribute 'default nil :height 140)

  ;; Add homebrew installed packages to load-path
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))

  ;; Toggle fullscreen mode
  (global-set-key [s-return] 'toggle-frame-fullscreen)

  ;; Allow hash command
  (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#"))))

;; Language specifics
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)

;; Make sure everything is in utf-8
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8)

;; Use auto-complete mode always
(global-auto-complete-mode)

;; Smex
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs")

(global-set-key (kbd "M-x") 'smex)

;; Compile bindings
(global-set-key (kbd "C-c C-k") 'compile)

;; Add line numbers
(global-linum-mode)
(setq linum-format "%d ")

;; Theming
;; Function to determine if we're running in graphics mode or not
;; (if (display-graphic-p))

(load-theme 'material t)
