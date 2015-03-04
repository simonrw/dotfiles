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

;; Packages

(defun load-config (file)
  (load-file (expand-file-name file "~/.emacs.d/config")))

(load-config "keys.el")
(load-config "visual.el")
(load-config "misc.el")
(load-config "packages.el")

; ;; Ido

(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
