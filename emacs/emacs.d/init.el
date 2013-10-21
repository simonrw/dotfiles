(require 'package)
(package-initialize)

(setq package-archives '(("melpa" .  "http://melpa.milkbox.net/packages/")
                         ))
(package-initialize)
(when (not package-archive-contents)
    (package-refresh-contents))

; Install packages
(setq package-list '(melpa
		     solarized-theme
		     haskell-mode
                     dash
                     markdown-mode))
(dolist (package package-list)
 (when (not (package-installed-p package))
  (package-install package)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco")))))


