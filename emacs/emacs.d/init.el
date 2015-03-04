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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4a60f0178f5cfd5eafe73e0fc2699a03da90ddb79ac6dbc73042a591ae216f03" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
