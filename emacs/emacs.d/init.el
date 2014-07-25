;; Use meta key on linux
(setq x-super-keysym 'meta)

;; Disable all bells
(setq ring-bell-function 'ignore)

;; Disable the startup pane
(setq inhibit-startup-message t)

;; Add the melpa repositories
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (package-refresh-contents)
  )

;; add auto indentation
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Set the path from the shell
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

;; List packages
(defvar my-packages '(better-defaults
		      clojure-mode
		      clojure-test-mode
                      evil
		      cider
                      multi-term))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
