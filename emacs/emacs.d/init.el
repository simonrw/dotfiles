(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install a given package"
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(require-package 'better-defaults)

(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;; Fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

;; Cleaner settings
(add-hook 'after-init-hook '(lambda ()
                              (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
                              (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
                              (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))))
(setq inhibit-splash-screen t)
(setq make-backup-files nil)

;; ido-mode
(ido-mode)

;; Themes
(require-package 'ir-black-theme)
(load-theme 'ir-black t)

;; Smooth scrolling
(require-package 'smooth-scrolling)
(setq smooth-scroll-margin 5)
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)

;; Ansi term - colours
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require-package 'ag)

(setq make-backup-files nil)

(require-package 'multi-term)

;; replace tab characters
(setq-default tab-width 4 indent-tabs-mode nil)

;; Indent on new line
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Make the hash key work for OSX
(if (eq system-type 'darwin)
    (global-set-key (kbd "M-3") '(lambda() (interactive) (insert "#"))))

;; Lisp navigation
(progn
  (require 'elisp-slime-nav)
  (defun my-lisp-hook ()
    (elisp-slime-nav-mode)
    (turn-on-eldoc-mode))
  (add-hook 'elisp-lisp-mode-hook 'my-lisp-hook))
(require-package 'paredit)

;; Clojure
(require-package 'clojure-mode)
(require-package 'cider)

(defun my/cider-mode-hooks ()
  (cider-turn-on-eldoc-mode))

(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'cider-mode-hook 'my/cider-mode-hooks)
(setq cider-repl-pop-to-buffer-on-connect t)

;; magit
(require-package 'magit)

;; Adoc
(add-hook 'adoc-mode-hook
          (progn
            (lambda () (buffer-face-mode t))))

;; Text completion
(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Auctex
(require-package 'auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)

;; ido mode
(ido-mode)

;; Quick eshell running
(defun eshell-here ()
  "Opens a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

;; ace-jump
(require-package 'ace-jump-mode)
(require 'ace-jump-mode)

;; Haskell mode
(require-package 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Evil mode
(require-package 'evil)
(evil-mode t)

(require-package 'evil-jumper)
(require-package 'evil-indent-textobject)
(require-package 'evil-surround)
(require-package 'evil-matchit)
(require-package 'evil-leader)

;; Keyboard mappings
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(global-evil-matchit-mode t)
(global-evil-surround-mode t)
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode t)
(setq evil-leader/in-all-states t)

;; Leader
(evil-leader/set-leader ",")
(evil-leader/set-key (kbd "f") 'fiplr-find-file)
(evil-leader/set-key (kbd "w") 'ace-jump-mode)
