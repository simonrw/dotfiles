;;; Init file

;;; Defaults
(blink-cursor-mode 0)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(dolist (m '(tooltip-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp m) (funcall m -1)))
(show-paren-mode 1)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8)
(set-default 'truncate-lines nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
(setq default-show-trailing-whitespace t)
(setq tramp-default-method "sshx")
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Disable visual bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; If computer specific file exists, source it
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Package loading is handled through use-package
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (use-package cl))

(setq use-package-always-ensure t)

;;; OSX
(when (eq system-type 'darwin)

  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)

  (if (boundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composotion-mode))

  (set-face-attribute 'default nil :family "Source Code Pro")

  (set-face-attribute 'default nil :height 120)

  (setq-default locate-command "mdfind")

  ;; Toggle fullscreen mode
  (global-set-key [m-return] 'toggle-frame-fullscreen)

  (when (display-graphic-p)
    (setq-default mac-emulate-three-button-mouse t)
    (global-set-key (kbd "M-`") 'other-frame)))

;; Get correct path from system shell
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-arguments '("-l" "-i"))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "RUST_SRC_PATH"))

;; Editorconfig
(use-package editorconfig
  :diminish ""
  :config
  (editorconfig-mode 1))

;; ido
(use-package ido
  :config
  (ido-mode t))

;; fzf
(use-package fzf
  :commands fzf)

;; flycheck
(use-package flycheck
  :commands global-flycheck-mode
  :diminish " ⓕ"
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk t))

;; Completion
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t))

;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-mode)))

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (use-package flycheck-rust
    :after flycheck
    :commands flycheck-rust-setup
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package lsp-mode
  :init
  (add-hook 'prog-mode-hook 'lsp-mode)
  :config
  (use-package lsp-flycheck
    :ensure f ; comes with lsp mode
    :after flycheck))

(use-package lsp-rust
  :after lsp-mode)

(use-package racer
  :commands racer-mode
  :diminish racer-mode
  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  :bind (:map rust-mode-map
	      ("M-." . racer-find-definition))
  :config
  (setq racer-rust-src-path "~/.cargo/rust-src/src")
  (racer-turn-on-eldoc)
  (use-package company-racer
    :config
    (add-to-list 'company-backends 'company-racer)
    (setq company-tooltip-align-annotations t)
    :bind (:map rust-mode-map
		("M-." . racer-find-definition))))

(use-package cargo
  :commands cargo-minor-mode
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

;; Configure C family of xlanguages
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq c-indent-level 4 c-basic-offset 4 c-default-style "linux")
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))))


;; Theming
(load-theme 'wombat t)

(provide 'init)
;;; init.el ends here
