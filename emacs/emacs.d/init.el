(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install a given package"
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Themes
(load-theme 'wombat t)

;; Smooth scrolling
(require-package 'smooth-scrolling)
(setq smooth-scroll-margin 5)
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)

(require-package 'ag)

(setq make-backup-files nil)

(require-package 'multi-term)

;; replace tab characters
(setq-default tab-width 4 indent-tabs-mode nil)

;; Indent on new line
(define-key global-map (kbd "RET") 'newline-and-indent)

(if (display-graphic-p)
     (scroll-bar-mode -1))
(tool-bar-mode -1)

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

;; Auctex
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; Adoc
(add-hook 'adoc-mode-hook
          (progn
            (lambda () (buffer-face-mode t))))

;; Evil mode
(require-package 'evil)
(evil-mode t)

(require-package 'evil-jumper)
(require-package 'evil-indent-textobject)
(require-package 'evil-surround)
(require-package 'evil-matchit)
(require-package 'evil-leader)

(global-evil-matchit-mode t)
(global-evil-surround-mode t)
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode t)
(setq evil-leader/in-all-states t)

;; Leader
(evil-leader/set-leader ",")
