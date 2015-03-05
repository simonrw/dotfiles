;; Marking text

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Indentation

(setq tab-width 2
      indent-tabs-mode nil)

;; Backup files

(setq make-backup-files nil)

;; Alias yes and no

(defalias 'yes-or-no-p 'y-or-n-p)

;; make the hash key work for OSX
(if (eq system-type 'darwin)
    (global-set-key (kbd "M-3") '(lambda() (interactive) (insert "#"))))

;; Misc

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

(setq tramp-default-method "ssh")
