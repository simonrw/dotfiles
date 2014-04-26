(setq package-list
      '(clojure-mode
	cider
	graphviz-dot-mode
	multi-term
	yaml-mode
	python
	python-mode
	puppet-mode
	ruby-mode
	rspec-mode
	evil
	smart-tab
	magit
	))


(setq package-archives
      '(("elpa" . "http://tromey.com/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

; Automatically install packages that are missing
; Note unwanted packages must be removed from ~/.emacs.d/elpa for the time being
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; Set the colour scheme
(load "~/.emacs.d/colourscheme/base16-default-theme.el" nil t)

; Configure some plugins
(setq multi-term-program "zsh")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; My custom settings
(ido-mode t) ; IDO mode, better completion
(setq ido-enable-flux-matching 1)
(setq ido-everywhere t)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message 1) ; cleaner startup
(define-key global-map (kbd "RET") 'newline-and-indent) ; autoindent by default
