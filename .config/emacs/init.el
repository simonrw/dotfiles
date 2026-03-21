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

(use-package mise
  :ensure t
  :config
  (global-mise-mode))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs
        '(go html javascript json lua nix python rust
          tsx typescript yaml))
  (treesit-auto-install-all)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (setq major-mode-remap-alist (treesit-auto--build-major-mode-remap-alist)))

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
               '((python-mode python-ts-mode) . ("uv" "run" "ty" "server")))
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer"))))

(use-package flymake-ruff
  :ensure t
  :init
  (setq flymake-ruff-program '("uv" "run" "ruff"))
  :hook ((python-mode . flymake-ruff-load)
         (python-ts-mode . flymake-ruff-load)))

;; In-buffer completion popup (like nvim's blink.cmp)
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (define-key corfu-map (kbd "C-n") #'corfu-next)
  (define-key corfu-map (kbd "C-p") #'corfu-previous)
  (define-key corfu-map (kbd "RET") #'corfu-insert)
  (define-key corfu-map (kbd "C-y") #'corfu-insert)
  (define-key corfu-map (kbd "C-e") #'corfu-quit))

;; Extra completion sources: file paths, buffer words, etc.
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

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

(use-package embark-consult
  :ensure t
  :after (embark consult))

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

;; Split window behavior
(setq split-window-below t)
(setq split-window-right t)

;; C-c prefix keybindings
(keymap-global-set "C-c w" #'save-buffer)
(keymap-global-set "C-c q" #'save-buffers-kill-terminal)
(keymap-global-set "C-c o" #'srw/load-config)
(keymap-global-set "C-c f" #'project-find-file)
(keymap-global-set "C-c F" #'consult-find)
(keymap-global-set "C-c SPC" #'consult-ripgrep)
;; LSP
(keymap-global-set "C-c r" #'eglot-rename)
(keymap-global-set "C-c y" #'eglot-format)
(keymap-global-set "C-c c a" #'eglot-code-actions)
(keymap-global-set "C-c d" #'flymake-show-buffer-diagnostics)
(keymap-global-set "C-c A" #'flymake-show-project-diagnostics)
(keymap-global-set "C-c S" #'consult-imenu)
(keymap-global-set "C-c s" #'consult-eglot-symbols)
;; Git
(keymap-global-set "C-c g c" #'magit-commit)
(keymap-global-set "C-c g d" #'magit-diff-dwim)
(keymap-global-set "C-c g w" #'magit-stage-file)
(keymap-global-set "C-c g r" #'magit-unstage-file)
(keymap-global-set "C-c g a" #'magit-commit-amend)
(keymap-global-set "C-c g s" #'magit-status)
(keymap-global-set "C-c g t" #'eglot-find-typeDefinition)
(keymap-global-set "C-c g b" #'magit-blame)
(keymap-global-set "C-c c p" (lambda () (interactive)
                                (clipboard-kill-ring-save (point-min) (point-max))
                                (message "Buffer copied to clipboard")))
(keymap-global-set "C-c j" #'consult-mark)
;; Toggles
(keymap-global-set "C-c t w" (lambda () (interactive) (visual-line-mode 'toggle)))
(keymap-global-set "C-c t n" (lambda () (interactive) (display-line-numbers-mode 'toggle)))
(keymap-global-set "C-c t s" (lambda () (interactive) (flyspell-mode 'toggle)))
(keymap-global-set "C-c t z" (lambda () (interactive) (visual-fill-column-mode 'toggle)))

(defun srw/load-config ()
  (interactive)
  (load-file "~/.config/emacs/init.el"))

;; LSP navigation in eglot buffers
(with-eval-after-load 'eglot
  (keymap-set eglot-mode-map "M-." #'xref-find-definitions)
  (keymap-set eglot-mode-map "M-?" #'xref-find-references)
  (keymap-set eglot-mode-map "C-c i" #'eglot-find-implementation))

;; Buffer switching and file browsing
(keymap-global-set "C-c b" #'consult-buffer)
(keymap-global-set "C-c -" #'dired-jump)

;; Git gutter signs
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (keymap-global-set "C-c h n" #'diff-hl-next-hunk)
  (keymap-global-set "C-c h p" #'diff-hl-previous-hunk))

;; GitHub integration
(use-package forge
  :ensure t
  :after magit
  :init
  (setq forge-add-default-bindings nil)
  :config
  (keymap-set magit-mode-map "'" #'forge-dispatch))

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

;; Keep default-directory at the project root
(defun srw/set-default-directory-to-project-root ()
  "Set default-directory to the project root if in a project."
  (when-let ((project (project-current))
             (root (project-root project)))
    (setq default-directory root)))

(add-hook 'find-file-hook #'srw/set-default-directory-to-project-root)

;; consult
(use-package consult
  :ensure t
  :after (vertico)
  :bind (("C-x b" . consult-buffer))
)

(use-package consult-eglot
  :ensure t
  :after (consult eglot))

(use-package server
  :ensure nil
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
	(server-start)))
