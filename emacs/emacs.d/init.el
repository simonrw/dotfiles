;; Init file

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

; Bootstrap straight.el
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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

(straight-use-package 'use-package)

(setq indent-tabs-mode nil)
(setq-default tab-width 4)

;;; OSX
(when (eq system-type 'darwin)

  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)

  (defun pbcopy ()
	(interactive)
	(call-process-region (point) (mark) "pbcopy")
	(setq deactivate-mark t))

  (defun pbpaste ()
	(interactive)
	(call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

  (defun pbcut ()
	(interactive)
	(pbcopy)
	(delete-region (region-beginning) (region-end)))

  (global-set-key (kbd "C-c c") 'pbcopy)
  (global-set-key (kbd "C-c v") 'pbpaste)
  (global-set-key (kbd "C-c x") 'pbcut)

  (menu-bar-mode t)

  (if (boundp 'mac-auto-operator-composition-mode)
	  (mac-auto-operator-composition-mode))

  (set-face-attribute 'default nil :family "Source Code Pro" :height 130)

  ;; Toggle fullscreen mode
  (global-set-key [m-return] 'toggle-frame-fullscreen)

  (when (display-graphic-p)
	(setq-default mac-emulate-three-button-mouse t)
	(global-set-key (kbd "M-`") 'other-frame)))

(use-package no-littering)

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

(use-package csharp-mode
  :mode "\\.cs\\'")

(use-package graphviz-dot-mode
  :mode "\\.dot\\'")

(use-package org
  :hook (org-mode-hook . variable-pitch-mode)
  :config
  (setq org-startup-indented t)
  (setq org-startup-folded "showall")
  (setq org-directory "~/Dropbox/org")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  (setq org-plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
	 (R . t)
	 (dot . t)
	 (ruby . t)
	 (python . t))))

(use-package python-pytest
  :init
  (add-hook 'python-mode
            (advice-add 'python-pytest-file :before
                        (lambda (&rest args)
                          (setq python-pytest-executable (+python-executable-find "pytest"))))))

(use-package org-preview-html
  :after (org))

(use-package org-bullets
  :after (org)
  :init
  (add-hook 'org-mode-hook (lambda () org-bullets-mode 1)))

(use-package org-roam
  :after (org)
  :hook (after-init-hook . org-roam-mode)
  :config
  (setq org-roam-directory "~/org-roam"))

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package dockerfile-mode
  :mode "Dockerfile")

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package php-mode
  :mode "\\.php\\'"
  :config
  (message "Loaded php mode"))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package vterm)

(load-theme 'tango-dark t)

(provide 'init)
;;; init.el ends here
