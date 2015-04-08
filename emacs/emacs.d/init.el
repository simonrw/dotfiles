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
                          monokai-theme))

(dolist (p srw/my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Theming
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes (quote ("a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#272822" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Menlo")))))
