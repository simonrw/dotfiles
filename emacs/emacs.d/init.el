;; Packages

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries
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
                          color-theme-solarized
                          monokai-theme
                          markdown-mode
                          ir-black-theme))

(dolist (p srw/my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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

