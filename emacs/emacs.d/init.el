;; Packages

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Install packages
(defvar srw/my-packages '(better-defaults
                          paredit
                          magit
                          smex
                          ido-ubiquitous
                          idle-highlight-mode
                          find-file-in-project
                          multi-term
                          web-mode
                          haskell-mode
                          ; Themes
                          color-theme-solarized
                          monokai-theme
                          markdown-mode
                          ir-black-theme))

(defun require-package (package &optional min-version no-refresh)
  "Install a given package, optionally requiring min-version."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(dolist (p srw/my-packages)
  (require-package p))

(define-key global-map (kbd "RET") 'newline-and-indent)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Source Code Pro")
  (set-face-attribute 'default nil :height 180)

  ;; Add homebrew installed packages to load-path
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))

  ;; Toggle fullscreen mode
  (global-set-key [s-return] 'toggle-frame-fullscreen)

  ;; Swap command and meta
  (setq ns-alternate-modifier 'super)
  (setq ns-command-modifier 'meta)

  ;; Allow hash command
  (global-set-key (kbd "s-3") '(lambda () (interactive) (insert "#"))))

;; Evil mode
(evil-mode)


;; Theming
(load-theme 'wombat t)
