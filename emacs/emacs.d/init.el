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

;; Font
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 160
                    :weight 'normal
                    :width 'normal)

;; Packages

(defun load-config (file)
  (load-file (expand-file-name file "~/.emacs.d/config")))

(load-config "packages.el")

;; Splash screen

(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Scroll/tool/menu bars

(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

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

;; make the hash key work for OSX
(if (eq system-type 'darwin)
    (global-set-key (kbd "M-3") '(lambda() (interactive) (insert "#"))))

;; Misc

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

; ;; Ido

(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
