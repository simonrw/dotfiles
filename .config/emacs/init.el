(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(setq org-agenda-files '("~/notes.org"))
(setq org-todo-keywords
	  '((sequence "TODO" "|" "DONE" "DROPPED")))

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(prefer-coding-system 'utf-8)

(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

(setq indent-tabs-mode nil)
(setq-default tab-width 4)

(setq inhibit-startup-echo-area-message (user-login-name))
(setq use-package-always-ensure t)


(setq frame-resize-pixelwise t)

(dolist
    (m '(tooltip-mode tool-bar-mode scroll-bar-mode menu-bar-mode blink-cursor-mode))
  (when (fboundp m) (funcall m -1)))


(use-package catppuccin-theme
             :ensure t
  :config
  (setq catppuccin-flavor 'mocha))

(use-package doom-themes
             :ensure t
             )

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
(setq mac-command-modifier      'meta
        ns-command-modifier       'meta
        mac-option-modifier       'super
        ns-option-modifier        'super
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none
        s-right-option-modifier  'none)
(use-package which-key
             :ensure t
  :config
  (which-key-mode))




(ido-mode 1)
(ido-everywhere 1)
(use-package smex
             :ensure t
  :config
  (smex-initialize)
  :bind
  ("M-x" . smex)
  ("C-c C-c M-x" . execute-extended-command))

(set-face-attribute 'default nil :family  "JetBrainsMono Nerd Font" :height 130)
(set-face-attribute 'variable-pitch nil :family "Helvetica Neue" :height 130)
(set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family) :height 110)


; key bindings
(global-set-key [(super a)] 'mark-whole-buffer)
(global-set-key [(meta v)] 'clipboard-yank)
(global-set-key [(super c)] 'kill-ring-save)
(global-set-key [(meta s)] 'save-buffer)
(global-set-key [(super l)] 'goto-line)
(global-set-key [(super w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(super z)] 'undo)


(use-package vterm
  :ensure t)

(use-package magit
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
	"w" 'save-buffer
    "q" 'save-buffers-kill-terminal
    "o" 'srw/load-config)
  (global-evil-leader-mode))

(defun srw/load-config ()
  (interactive)
  (load-file "~/.config/emacs/init.el"))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(keymap-global-set "C-c a" #'org-agenda)

(with-eval-after-load 'org
  (setq org-startup-indented nil)
  (add-hook 'org-mode-hook #'visual-line-mode))

;; org mode configuration
(use-package evil-org
             :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle))
