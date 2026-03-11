(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(setq org-agenda-files '("~/notes.org"))
(setq org-todo-keywords
	  '((sequence "TODO" "|" "DONE" "DROPPED")))

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror 'nomessage)

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

;; macos setting, move when we have os detection
(set-frame-parameter nil 'ns-transparent-titlebar t)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))


(setq frame-resize-pixelwise t)

(dolist
    (m '(tooltip-mode tool-bar-mode scroll-bar-mode menu-bar-mode blink-cursor-mode))
  (when (fboundp m) (funcall m -1)))


(use-package catppuccin-theme
             :ensure t)

(use-package doom-themes
  :ensure t
  )

(use-package auto-dark
             :ensure t
  :custom
  (auto-dark-themes '((catppuccin) (catppuccin)))
  :init
  (defun my/server-auto-dark (frame)
    (with-selected-frame frame
      (when (display-graphic-p)
        (auto-dark-mode 1)
        (remove-hook 'after-make-frame-functions #'my/server-auto-dark))))
  :config
  (setq auto-dark-allow-osascript t)
  (add-hook 'auto-dark-dark-mode-hook
            (lambda ()
              (setq catppuccin-flavor 'mocha)
              (catppuccin-reload)))
  (add-hook 'auto-dark-light-mode-hook
            (lambda ()
              (setq catppuccin-flavor 'latte)
              (catppuccin-reload)))
  (add-hook 'after-make-frame-functions #'my/server-auto-dark)
  (auto-dark-mode 1))

(use-package exec-path-from-shell
             :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

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

(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("ty" "server"))))

(use-package flymake-ruff
  :ensure t
  :hook ((python-mode . flymake-ruff-load)
         (python-ts-mode . flymake-ruff-load)))

(use-package ivy
  :ensure t
  :init
  (setopt ivy-use-virtual-buffers t)
  :config
  (ivy-mode))

(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode)
  :bind (("M-x" . counsel-M-x)
		 ("C-x C-f" . counsel-find-file)))

(use-package swiper
  :ensure t
  :after counsel)


(set-face-attribute 'default nil :family  "Lilex" :height 130)
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
  :after (evil swiper projectile)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
	"w" 'save-buffer
    "q" 'save-buffers-kill-terminal
    "o" 'srw/load-config
    "f" 'projectile-find-file
    "<SPC>" 'swiper-isearch)
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

(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode))

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

(use-package moody
  :ensure t
  :config
  (setq-default mode-line-format
				`(""
				  mode-line-front-space
				  mode-line-client
				  mode-line-frame-identification
				  mode-line-buffer-identification
				  " "
				  mode-line-position
				  (vc-mode vc-mode)
				  mode-line-modes
				  mode-line-end-spaces))
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(define-key global-map (kbd "s-p") nil)

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/dev" "~/dev/forks" "~/work/localstack"))
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map))

(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-anthropic-claude-environment (agent-shell-make-environment-variables :inherit-env t))
  (setq agent-shell-anthropic-claude-acp-command
		'("bunx" "@zed-industries/claude-agent-acp")))

(use-package rg
  :ensure t)

;; speed up tramp
;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;; taken from https://www.youtube.com/watch?v=51eSeqcaikM
(recentf-mode 1)
(setq history-length 25)
(savehist-mode 1)
(save-place-mode 1)
(setq use-dialog-box nil)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; consult
(use-package consult
  :ensure t
  :after (evil)
  :bind ("C-x b" . consult-buffer)
  :config
  (evil-define-key 'normal 'global (kbd "g b") 'consult-buffer))

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))
