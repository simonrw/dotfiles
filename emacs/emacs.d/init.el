;; User details
(setq user-full-name "Simon Walker")
(setq user-email-address "s.r.walker101@googlemail.com")

;; Environment

;; Fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))
(require 'cl)

;; Package management

(load "package")
(package-initialize)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-archive-enable-alist '(("melpa" deft magit)))

;; Default packages
(defvar srwalker101/packages
  '(haskell-mode
    smex
    ir-black-theme
    markdown-mode
    auto-complete
    evil
    evil-leader
    writeroom-mode
    magit
    auctex
    writegood-mode
    clojure-mode))

;; Install default packages

(defun srwalker101/packages-installed-p ()
  (loop for pkg in srwalker101/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (srwalker101/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg srwalker101/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Start up options

;; Font
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 160
                    :weight 'normal
                    :width 'normal)

;; Splash screen

(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Scroll/tool/menu bars

(when window-system
    (scroll-bar-mode -1))
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Marking text

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Display settings
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Indentation

(setq tab-width 2
      indent-tabs-mode nil)

;; Backup files

(setq make-backup-files nil)

;; Alias yes and no

(defalias 'yes-or-no-p 'y-or-n-p)

;; Keybindings

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;; Evil

(require 'evil)
(evil-mode t)

;; Evil leader

(require 'evil-leader)
(evil-leader/set-leader ",")

;; Make the hash key work for OSX
(if (eq system-type 'darwin)
    (global-set-key (kbd "M-3") '(lambda() (interactive) (insert "#"))))

;; Misc

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; Packages

;; Smex

(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Ido

(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; Auto-complete

(require 'auto-complete-config)
(ac-config-default)

;; AucTeX

(setq-default TeX-master nil) ; Query for master file

;; Language settings

;; Markdown

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (visual-line-mode t)
	    (writegood-mode t)))
(setq markdown-command "pandoc --smart --standalone -f markdown -t html")

;; Theme

(load-theme 'wombat t)
