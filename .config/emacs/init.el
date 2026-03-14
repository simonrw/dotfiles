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

(setq-default window-combination-resize t)
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

(when (eq system-type 'darwin)
  (set-frame-parameter nil 'ns-transparent-titlebar t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))


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
  :if (eq system-type 'darwin)
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; macOS-specific settings
;; https://github.com/doomemacs/doomemacs/blob/master/modules/os/macos/config.el
(when (eq system-type 'darwin)
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
        mac-right-option-modifier 'none
        s-right-option-modifier  'none))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure)
         (json-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (markdown-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("uv" "run" "ty" "server"))))

(use-package flymake-ruff
  :ensure t
  :init
  (setq flymake-ruff-program '("uv" "run" "ruff"))
  :hook ((python-mode . flymake-ruff-load)
         (python-ts-mode . flymake-ruff-load)))

;; Completion stack: Vertico + Orderless + Marginalia + Consult + Embark
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)))


(when (display-graphic-p)
  (set-face-attribute 'default nil :family "Lilex" :height 130)
  (set-face-attribute 'variable-pitch nil
                      :family (if (eq system-type 'darwin) "Helvetica Neue" "Sans Serif")
                      :height 130)
  (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family) :height 110))


;; macOS Cmd-key bindings (super = Cmd after modifier remap)
(when (eq system-type 'darwin)
  (global-set-key [(super a)] 'mark-whole-buffer)
  (global-set-key [(meta v)] 'clipboard-yank)
  (global-set-key [(super c)] 'kill-ring-save)
  (global-set-key [(meta s)] 'save-buffer)
  (global-set-key [(super l)] 'goto-line)
  (global-set-key [(super w)]
                  (lambda () (interactive) (delete-window)))
  (global-set-key [(super z)] 'undo))


(use-package vterm
  :ensure t)

(use-package magit
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  :config
  (evil-mode)
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "w" 'save-buffer
    "q" 'save-buffers-kill-terminal
    "o" 'srw/load-config
    "f" 'project-find-file
    "F" 'consult-find
    "<SPC>" 'consult-ripgrep
    ;; LSP
    "r" 'eglot-rename
    "y" 'eglot-format
    "a" 'eglot-code-actions
    "d" 'flymake-show-buffer-diagnostics
    "A" 'flymake-show-project-diagnostics
    "S" 'consult-imenu
    "s" 'consult-eglot-symbols
    ;; Git
    "gc" 'magit-commit
    "gd" 'magit-diff-dwim
    "gw" 'magit-stage-file
    "gr" 'magit-unstage-file
    "ga" 'magit-commit-amend
    "gt" 'eglot-find-typeDefinition
    "cp" (lambda () (interactive)
           (clipboard-kill-ring-save (point-min) (point-max))
           (message "Buffer copied to clipboard"))
    ;; Toggles
    "tw" (lambda () (interactive) (visual-line-mode 'toggle))
    "tn" (lambda () (interactive) (display-line-numbers-mode 'toggle))
    "tr" (lambda () (interactive)
           (if (eq display-line-numbers 'relative)
               (setq display-line-numbers t)
             (setq display-line-numbers 'relative)))
    "ts" (lambda () (interactive) (flyspell-mode 'toggle))
    "tz" (lambda () (interactive) (visual-fill-column-mode 'toggle))
    "tb" 'magit-blame)
  (global-evil-leader-mode))

(defun srw/load-config ()
  (interactive)
  (load-file "~/.config/emacs/init.el"))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; LSP navigation in eglot buffers
(with-eval-after-load 'eglot
  (evil-define-key 'normal eglot-mode-map
    (kbd "gd") 'xref-find-definitions
    (kbd "gr") 'xref-find-references
    (kbd "gi") 'eglot-find-implementation))

;; Git status, file browsing, copy file, quickfix
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "gs") 'magit-status)
  (evil-define-key 'normal 'global (kbd "-") 'dired-jump)
  (evil-define-key 'normal 'global (kbd "Q")
    (lambda () (interactive)
      (if-let ((win (get-buffer-window "*Flymake diagnostics*")))
          (delete-window win)
        (flymake-show-buffer-diagnostics))))
)

;; Git gutter signs
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (evil-define-key 'normal 'global (kbd "]c") 'diff-hl-next-hunk)
  (evil-define-key 'normal 'global (kbd "[c") 'diff-hl-previous-hunk))

;; GitHub integration
(use-package forge
  :ensure t
  :after magit)

(keymap-global-set "C-c a" #'org-agenda)

(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-center-text t)
  :hook
  (org-mode . visual-fill-column-mode)
  (markdown-mode . visual-fill-column-mode))

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


(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-anthropic-claude-environment (agent-shell-make-environment-variables :inherit-env t))
  (setq agent-shell-anthropic-claude-acp-command
		'("bunx" "@zed-industries/claude-agent-acp")))

(use-package markdown-mode
  :ensure t)

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
  :bind (("C-x b" . consult-buffer))
  :config
  (evil-define-key 'normal 'global (kbd "g b") 'consult-buffer))

(use-package consult-eglot
  :ensure t
  :after (consult eglot))
