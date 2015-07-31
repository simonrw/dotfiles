;; Packages

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Install packages
(defvar srw/my-packages '(better-defaults
                          paredit
                          magit
                          smex
                          ido-ubiquitous
                          idle-highlight-mode
                          find-file-in-project
                          multi-term
                          web-mode
                          haskell-mode
                          ; Evil packages
                          evil
                          evil-commentary
                          evil-matchit
                          evil-leader
                          ; Themes
                          color-theme-solarized
                          monokai-theme
                          markdown-mode
                          ir-black-theme))

(defun require-package (package &optional min-version no-refresh)
  "Install a given package, optionally requiring min-version."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(dolist (p srw/my-packages)
  (require-package p))

(define-key global-map (kbd "RET") 'newline-and-indent)

;; Evil mode
(setq vil-search-module 'evil-search
      evil-want-C-U-scroll t
      evil-want-C-w-in-emacs-state t)
(evil-commentary-mode)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(require 'evil-matchit)
(global-evil-matchit-mode 1)
;; Finally enable evil mode
(evil-mode t)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Inconsolata")
  (set-face-attribute 'default nil :height 180)

  ;; Toggle fullscreen mode
  (global-set-key [s-return] 'toggle-frame-fullscreen)

  ;; Swap command and meta
  (setq ns-alternate-modifier 'super)
  (setq ns-command-modifier 'meta)

  ;; Allow hash command
  (global-set-key (kbd "s-3") '(lambda () (interactive) (insert "#"))))


;; Theming
(load-theme 'wombat t)
