(setq package-list
      '(multi-term
        vimrc-mode
        python-mode
        magit))

(setq package-archives
      '(("elpa" . "http://tromey.com/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

; Automatically install packages that are missing
; Note unwanted packages must be removed from ~/.emacs.d/elpa for the time being
(unless package-archive-contents
  (package-refresh-contents))

(if
    (equal window-system 'ns)
    (menu-bar-mode 1)
  (menu-bar-mode 0))

(global-set-key (kbd "C-o") 'other-window)
;; Re-enable hash key on mac os x
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
