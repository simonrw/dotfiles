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

(load-theme 'solarized t)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Menlo")
  (set-face-attribute 'default nil :height 160)

  ;; Toggle fullscreen mode
  (global-set-key [s-return] 'toggle-frame-fullscreen)

  ;; Swap command and meta
  (setq ns-alternate-modifier 'super)
  (setq ns-command-modifier 'meta))

