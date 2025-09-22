(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'org-agenda-files "~/notes.org" t)

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
  (auto-dark-themes '((catppuccin) (leuven)))
  :config
  (setq auto-dark-allow-osascript t)
  (auto-dark-mode))
