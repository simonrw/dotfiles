;; Font
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 160
                    :weight 'normal
                    :width 'normal)

;; Splash screen

(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Scroll/tool/menu bars

(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

;; Display settings
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
