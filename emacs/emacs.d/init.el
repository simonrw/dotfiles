(setq package-list
      '(clojure-mode
	graphviz-dot-mode
	multi-term
	))


(setq package-archives
      '(("elpa" . "http://tromey.com/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))    
			   
(load "~/.emacs.d/colourscheme/base16-default-theme.el" nil t)
