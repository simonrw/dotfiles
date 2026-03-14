## 1. Remove Old Packages

- [x] 1.1 Remove ivy, counsel, swiper use-package blocks and all references
- [x] 1.2 Remove smex package and references
- [x] 1.3 Remove projectile use-package block and all references (keybindings, config)

## 2. Add Modern Completion Stack

- [x] 2.1 Add vertico with use-package (enable vertico-mode)
- [x] 2.2 Add orderless with use-package (set completion-styles)
- [x] 2.3 Add marginalia with use-package (enable marginalia-mode)
- [x] 2.4 Add consult with use-package (bind consult-ripgrep, consult-buffer, consult-find, consult-imenu)
- [x] 2.5 Add embark with use-package (bind embark-act, embark-dwim)
- [x] 2.6 Add consult-eglot for workspace symbols

## 3. Replace Projectile with project.el

- [x] 3.1 Bind SPC f to project-find-file
- [x] 3.2 Ensure consult-ripgrep uses project-root for SPC SPC

## 4. Evil Leader Keybinding Parity

- [x] 4.1 Bind SPC SPC to consult-ripgrep
- [x] 4.2 Bind SPC f to project-find-file
- [x] 4.3 Bind SPC F to consult-find
- [x] 4.4 Bind gb to consult-buffer in evil normal
- [x] 4.5 Bind cp to copy-whole-buffer-to-clipboard (custom function)
- [x] 4.6 Keep SPC w (save), SPC q (quit), SPC o (reload) as-is

## 5. LSP Keybindings

- [x] 5.1 Bind gd to xref-find-definitions in eglot buffers via evil-define-key
- [x] 5.2 Bind gr to xref-find-references in eglot buffers
- [x] 5.3 Bind gi to eglot-find-implementation in eglot buffers
- [x] 5.4 Bind SPC gt to eglot-find-typeDefinition
- [x] 5.5 Bind SPC r to eglot-rename
- [x] 5.6 Bind SPC y to eglot-format
- [x] 5.7 Bind SPC a to eglot-code-actions
- [x] 5.8 Bind SPC d to flymake-show-buffer-diagnostics
- [x] 5.9 Bind SPC A to flymake-show-project-diagnostics
- [x] 5.10 Bind SPC S to consult-imenu (document symbols)
- [x] 5.11 Bind SPC s to consult-eglot-symbols (workspace symbols)

## 6. Git Workflow

- [x] 6.1 Bind gs to magit-status in evil normal
- [x] 6.2 Bind SPC gc to magit-commit
- [x] 6.3 Bind SPC gd to magit-diff-dwim
- [x] 6.4 Bind SPC gw to magit-stage-file
- [x] 6.5 Bind SPC gr to magit-unstage-file
- [x] 6.6 Bind SPC ga to magit-commit-amend
- [x] 6.7 Add diff-hl package, enable diff-hl-mode globally
- [x] 6.8 Bind ]c to diff-hl-next-hunk, [c to diff-hl-previous-hunk in evil normal
- [x] 6.9 Add forge package (loaded after magit)

## 7. File Browsing & Toggles

- [x] 7.1 Bind - to dired-jump in evil normal
- [x] 7.2 Bind yow to toggle visual-line-mode
- [x] 7.3 Bind yon to toggle display-line-numbers-mode
- [x] 7.4 Bind yor to toggle relative line numbers
- [x] 7.5 Bind yos to toggle flyspell-mode
- [x] 7.6 Bind yoz to toggle visual-fill-column-mode
- [x] 7.7 Bind yob to toggle magit-blame
- [x] 7.8 Bind Q to toggle flymake diagnostics buffer

## 8. Platform Support

- [x] 8.1 Wrap macOS-specific settings (ns-*, mdfind, titlebar) in (when (eq system-type 'darwin) ...)
- [x] 8.2 Wrap exec-path-from-shell to only run on macOS
- [x] 8.3 Make font configuration platform-aware
