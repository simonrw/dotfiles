(setq package-list
      '(clojure-mode
	graphviz-dot-mode
	multi-term
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
