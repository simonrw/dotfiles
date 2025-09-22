(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq org-agenda-files '("~/notes.org"))

(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

(setq inhibit-startup-echo-area-message (user-login-name))


(setq frame-resize-pixelwise t)

(tool-bar-mode -1)

(use-package evil
  :ensure t

  :config
  (evil-mode)
  (evil-set-initial-state 'vterm-mode 'emacs))

;; org mode configuration
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle))

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha))

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((catppuccin) (doom-one-light)))
  :config
  (setq auto-dark-allow-osascript t)
  (auto-dark-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))

;; macos settings
;; https://github.com/doomemacs/doomemacs/blob/master/modules/os/macos/config.el
(setq locate-command "mdfind")
(setq ns-use-native-fullscreen nil)
(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)
(and (or (daemonp)
         (display-graphic-p))
     (require 'ns-auto-titlebar nil t)
     (ns-auto-titlebar-mode +1))
(setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none
        s-right-option-modifier  'none)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package doom-themes
  :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
