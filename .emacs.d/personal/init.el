(use-package batppuccin
  :ensure t
  :init
  (load-theme 'batppuccin-latte :no-confirm))

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((batppuccin-mocha) (batppuccin-latte)))
  :init
  (auto-dark-mode))
