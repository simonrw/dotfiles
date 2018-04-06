;;; Init file

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

(setq use-package-always-ensure t)

(setq indent-tabs-mode nil)
(setq-default tab-width 4)

;;; OSX
(when (eq system-type 'darwin)

  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)

  (menu-bar-mode t)

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

;; Show available keys on pause
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

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
  :init (setq markdown-command "multimarkdown"))

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

;; Theming
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

(load "server")
(unless (eq (server-running-p) t)
  (server-start))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)
   (ruby . t)
   (rust . t)
   (python . t)))

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(setq org-startup-indented t)
(setq org-startup-folded "showall")
(setq org-directory "~/Dropbox/org")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

(setq org-plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")
(setq org-src-fontify-natively t)

(use-package evil
  :ensure t
  :config
  (evil-mode t))

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package go-mode)

(use-package elixir-mode)

(defun todo ()
  (interactive)
  (find-file (concat org-directory "/todo.org")))

(global-visual-line-mode t)

(provide 'init)
;;; init.el ends here
