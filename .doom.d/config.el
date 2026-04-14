;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; --- Fonts ---
(setq doom-font (font-spec :family "Lilex" :size 13)
      doom-variable-pitch-font (font-spec :family "Helvetica Neue" :size 13))

;;; --- Theme: catppuccin with auto dark/light ---
(setopt custom-safe-themes t)
(setopt catppuccin-flavor 'mocha)

;;; --- Handle shells
(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell "fish")
(setq-default explicit-shell-file-name "fish")


(use-package! auto-dark
  :defer t
  :init
  (setq doom-theme nil)
  (defun my-auto-dark-init-h ()
    (auto-dark-mode)
    (remove-hook 'server-after-make-frame-hook #'my-auto-dark-init-h)
    (remove-hook 'after-init-hook #'my-auto-dark-init-h))
  (let ((hook (if (daemonp)
                  'server-after-make-frame-hook
                'after-init-hook)))
    (add-hook hook #'my-auto-dark-init-h -95))
  :config
  (setq auto-dark-allow-osascript t)
  ;; Use setq to avoid the defcustom :set function calling load-theme during
  ;; auto-dark.el loading, where load-file-name is still bound to auto-dark.elc
  ;; and catppuccin-definitions.el can't be found.
  (setq auto-dark-themes '((catppuccin) (catppuccin)))
  (add-hook 'auto-dark-dark-mode-hook
            (lambda ()
              (setq catppuccin-flavor 'mocha)
              (catppuccin-reload)))
  (add-hook 'auto-dark-light-mode-hook
            (lambda ()
              (setq catppuccin-flavor 'latte)
              (catppuccin-reload))))

;;; --- UI ---
(setq display-line-numbers-type nil)

;;; --- macOS ---
(when (featurep :system 'macos)
  (setq mac-right-option-modifier 'none))

;;; --- Disable smartparens ---
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;; --- Org ---
(setq org-directory "~/org/")
(setq org-agenda-files '("~/notes.org"))
(after! org
  (setq org-todo-keywords '((sequence "TODO" "|" "DONE" "DROPPED"))))

;;; --- LSP (eglot) ---
(after! eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("uv" "run" "ty" "server")))
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer"))))

;;; --- Flymake-ruff ---
(use-package! flymake-ruff
  :hook ((python-mode . flymake-ruff-load)
         (python-ts-mode . flymake-ruff-load))
  :init
  (setq flymake-ruff-program '("uv" "run" "ruff")))

;;; --- Mise (version manager) ---
(use-package! mise
  :config
  (global-mise-mode))

;;; --- Agent Shell (Claude AI) ---
(after! agent-shell
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))
  (setq agent-shell-anthropic-claude-acp-command
        '("bunx" "@zed-industries/claude-agent-acp")))

;;; --- Tramp ---
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(after! tramp
  (after! compile
    (remove-hook 'compilation-mode-hook
                 #'tramp-compile-disable-ssh-controlmaster-options)))

(setq magit-tramp-pipe-stty-settings 'pty)

;;; --- Project root as default-directory ---
(defun srw/set-default-directory-to-project-root ()
  "Set default-directory to the project root if in a project."
  (when-let ((project (project-current))
             (root (project-root project)))
    (setq default-directory root)))

(add-hook 'find-file-hook #'srw/set-default-directory-to-project-root)

;;; --- Emacs server ---
(after! server
  (unless (daemonp)
    (unless (server-running-p)
      (server-start))))

;;; --- Forge ---
(after! forge
  (map! :map magit-mode-map "'" #'forge-dispatch))

;;; --- Custom keybindings ---

;; diff-hl hunk navigation
(map! :n "]h" #'diff-hl-next-hunk
      :n "[h" #'diff-hl-previous-hunk)

;; Leader bindings
(map! :leader
      :desc "Copy buffer to clipboard"
      "b y" (cmd! (clipboard-kill-ring-save (point-min) (point-max))
                  (message "Buffer copied to clipboard"))
      :desc "Toggle zen mode"
      "t z" #'+zen/toggle)
