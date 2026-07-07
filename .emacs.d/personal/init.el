(use-package batppuccin
  :ensure t
  :init
  (load-theme 'batppuccin-latte :no-confirm))

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((batppuccin-mocha) (batppuccin-latte)))
  :init
  (auto-dark-mode))

(defvar srw-default-font-height 130
  "Default global font height, measured in tenths of a point.")

(defvar srw-font-height-step 10
  "Amount to change the global font height by.")

(defvar srw-current-font-height srw-default-font-height
  "Current global font height.")

(defun srw-set-font-height (height)
  "Set the global font HEIGHT, measured in tenths of a point."
  (interactive "nFont height: ")
  (setq srw-current-font-height height)
  (set-face-attribute 'default nil :height height)
  (message "Font height: %s" height))

(defun srw-increase-font-height ()
  "Increase the global font height."
  (interactive)
  (srw-set-font-height (+ srw-current-font-height srw-font-height-step)))

(defun srw-decrease-font-height ()
  "Decrease the global font height."
  (interactive)
  (srw-set-font-height (max srw-font-height-step
                            (- srw-current-font-height srw-font-height-step))))

(defun srw-reset-font-height ()
  "Reset the global font height to `srw-default-font-height'."
  (interactive)
  (srw-set-font-height srw-default-font-height))

(srw-set-font-height srw-default-font-height)

(global-set-key (kbd "C-+") #'srw-increase-font-height)
(global-set-key (kbd "C-=") #'srw-increase-font-height)
(global-set-key (kbd "C--") #'srw-decrease-font-height)
(global-set-key (kbd "C-0") #'srw-reset-font-height)

(use-package vterm :ensure t)

(defun srw-update-buffer ()
  "Save the current buffer only when it has unsaved changes."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer)))

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

(defun srw-project-root ()
  "Return the best project root for the current buffer."
  (let ((root (or (when (fboundp 'projectile-project-root)
                    (ignore-errors (projectile-project-root)))
                  (when (fboundp 'project-current)
                    (when-let ((project (project-current nil)))
                      (project-root project)))
                  (vc-root-dir))))
    (when root
      (file-name-as-directory (expand-file-name root)))))

(defun srw-set-default-directory-to-project-root ()
  "Make commands in this buffer run from the project root."
  (when-let ((root (srw-project-root)))
    (setq-local default-directory root)))

(add-hook 'find-file-hook #'srw-set-default-directory-to-project-root)

(defun srw-git-files (root)
  "Return git-controlled files for ROOT, including untracked files."
  (let ((default-directory root))
    (when (and (executable-find "git")
               (zerop (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree")))
      (process-lines "git" "ls-files" "--exclude-standard" "--cached" "--others"))))

(defun srw-find-git-file ()
  "Fuzzy find a git file from the project root."
  (interactive)
  (let* ((root (or (srw-project-root) default-directory))
         (files (srw-git-files root)))
    (unless files
      (user-error "No git files found"))
    (find-file (expand-file-name
                (completing-read "Git file: " files nil t)
                root))))

(with-eval-after-load 'evil-leader
  (evil-leader/set-key
    "f" #'srw-find-git-file
    "w" #'srw-update-buffer))

;; set up org
(setq org-agenda-files (quote ("~/notes")))

;; disable line number mode
(global-display-line-numbers-mode -1)

;; always start the server
(server-start)
