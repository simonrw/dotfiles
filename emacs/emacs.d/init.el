;; Add the melpa repositories
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  ;; List packages
  (defvar my-packages '(better-defaults
                        clojure-mode
                        clojure-test-mode
                        evil
                        evil-leader
                        cider
                        projectile
                        multi-term))

  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))

  )

;; Always use utf8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use meta key on linux
(setq x-super-keysym 'meta)

;; Disable all bells
(setq ring-bell-function 'ignore)

;; Disable the startup pane
(setq inhibit-startup-message t)

;; add auto indentation
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Set the path from the shell
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

(require 'projectile)
(projectile-global-mode)

(require 'icomplete)

;; Use nicer buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
