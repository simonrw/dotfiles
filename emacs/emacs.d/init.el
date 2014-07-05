(setq package-list
      '(multi-term
        clojure-mode
        python-mode
        evil
        cider
        markdown-mode
        graphviz-dot-mode
        magit))

(setq package-archives
      '(("elpa" . "http://tromey.com/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(package-initialize)

;; Disable bells completely
(setq ring-bell-function 'ignore)

; Automatically install packages that are missing
; Note unwanted packages must be removed from ~/.emacs.d/elpa for the time being
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; Configure some plugins
(setq multi-term-program "zsh")

; Set ido mode
(ido-mode)
(ido-everywhere)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("38c4fb6c8b2625f6307f3dde763d5c61d774d854ecee9c5eb9c5433350bc0bef" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(visible-bell nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; My custom settings
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message 1) ; cleaner startup
(define-key global-map (kbd "RET") 'newline-and-indent) ; autoindent by default
(global-subword-mode t)


(global-set-key (kbd "\C-x\C-z") 'magit-status)

;; no tabs
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default default-tab-width 4)

;; if running in terminal, disable the menu bar
(if (equal window-system 'ns)
    (menu-bar-mode 1)
  (menu-bar-mode 0))

(global-set-key (kbd "C-o") 'other-window)
;; Re-enable hash key on mac os x
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; Use menlo font where possible
(when (member "Menlo" (font-family-list))
  (set-face-attribute 'default nil :font "Menlo"))
