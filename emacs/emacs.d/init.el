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
    flycheck-rust
    cargo
    evil
    py-autopep8
	yaml-mode
    dumb-jump
	smex
    markdown-mode
    clang-format
    cmake-mode
	adoc-mode
	fzf
    multi-term
	clojure-mode
	cider
	paredit
	magit
	ansible
	editorconfig
	jinja2-mode
	rust-mode
    racer
	company
    exec-path-from-shell))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      srw/my-packages)

;; Set up the path variable
;; Configure the startup to not check my terrible
;; .zshrc file
(setq exec-path-from-shell-check-startup-files nil)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Settings
(setq inhibit-startup-message t)
(setq visible-bell nil)

(define-key global-map (kbd "RET") 'newline-and-indent)
(global-visual-line-mode t)

;; Hooks
(add-hook 'markdown-mode-hook
		  (auto-fill-mode))

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Use ssh for tramp instead of scp
(setq tramp-default-method "ssh")

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Source Code Pro")
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

;; Set up rust
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Strip trailing whitespace for all files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable dumb jump
(dumb-jump-mode)

;; Make sure everything is in utf-8
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8)

;; Disable word wrap (truncating lines in emacs speak)
(set-default 'truncate-lines nil)

;; Set up fzf
;; Add the fzf dir to the PATH
;; (setenv "PATH" (concat
				;; (getenv "HOME") "/.fzf/bin:" (getenv "PATH")))
;; (setq exec-path
	  ;; (append
	   ;; '((concat (getenv "HOME") "/.fzf/bin") exec-path)))

;; Smex
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs")

(global-set-key (kbd "M-x") 'smex)

;; Open files in an existing frame instead of a new frame each time
(setq ns-pop-up-frames nil)

;; Compile bindings
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-c C-r") 'recompile)

;; Make sure line numbers are not on
(global-linum-mode 0)

;; Always use spaces
(setq-default indent-tabs-mode nil)

;; Stop any bells
(setq ring-bell-function 'ignore)

;; Mouse control (BAD!)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; Set up rust and racer
(setq racer-rust-src-path "~/.cargo/rust-src/src/")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; Set up backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq atuo-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Enable automatically wrapping search
;; Taken from https://stackoverflow.com/a/287067/56711
(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))
;; Theming
(load-theme 'wombat t)
