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
                          evil
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

;; Evil mode
(setq vil-search-module 'evil-search
      evil-want-C-U-scroll t
      evil-want-C-w-in-emacs-state t)
(evil-mode t)

;; Theming
(set-frame-parameter nil 'background-mode 'dark)

(load-theme 'ir-black t)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Menlo")
  (set-face-attribute 'default nil :height 160)

  ;; Toggle fullscreen mode
  (global-set-key [s-return] 'toggle-frame-fullscreen)

  ;; Swap command and meta
  (setq ns-alternate-modifier 'super)
  (setq ns-command-modifier 'meta))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
