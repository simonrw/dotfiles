;; Init file

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

;;; Defaults
(blink-cursor-mode 0)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(dolist (m '(tooltip-mode tool-bar-mode scroll-bar-mode menu-bar-mode))
  (when (fboundp m) (funcall m -1)))
(show-paren-mode 1)
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8)
(set-default 'truncate-lines nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-visual-line-mode t)
(set-fringe-mode 10)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Enable nicer window moving
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Disable visual bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; If computer specific file exists, source it (any settings changed by M-x customize)
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Packages
(require 'package)

;; Add Melpa to package archives
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Package loading is handled through use-package
(setq package-enable-at-startup t)
(package-initialize)

; Initialise package archive
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(setq indent-tabs-mode nil)
(setq-default tab-width 4)

;;; OSX
(when (eq system-type 'darwin)

  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)

  (menu-bar-mode t)

  (if (boundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode))

  (set-face-attribute 'default nil :family "Source Code Pro" :height 130)

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
  :config
  (editorconfig-mode 1))

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk t))

;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-header-scaling t))

(use-package rust-mode
  :mode "\\.rs\\'"
  :bind (("C-c C-t" . cargo-process-check)
         ("C-c C-w" . cargo-process-test)
         ("C-c C-r" . cargo-process-run))
  :config
  (setq rust-format-on-save t))

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

(use-package cmake-mode
  :mode (("CMakeLists.txt" . cmake-mode)))

(use-package csharp-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)
   (dot . t)
  (ruby . t)
   (python . t)))

(setq org-startup-indented t)
(setq org-startup-folded "showall")
(setq org-directory "~/Dropbox/org")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

(setq org-plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")
(setq org-src-fontify-natively t)

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package elixir-mode)

(defun todo ()
  (interactive)
  (find-file (concat org-directory "/todo.org")))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda ()
							 org-bullets-mode 1)))
(use-package fzf)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dockerfile-mode)

(use-package yaml-mode)

(use-package php-mode)

(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package direnv
  :config
  (direnv-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (evil-set-leader 'normal (kbd ","))

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-define-key 'normal 'global (kbd "<leader>f") 'fzf-git-files)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package helm
  :config
  (helm-mode 1))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'helm))
  :bind-keymap
  ("C-c C-p" . projectile-command-map))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
		 ("C-x b" . counsel-ibuffer)
		 ("C-x C-f" . counsel-find-file)
		 :map minibuffer-local-map
		 ("C-r" . 'counsel-minibuffer-history)))

(load-theme 'wombat t)

(provide 'init)
;;; init.el ends here
