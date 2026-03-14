## Why

Switching primary editor from neovim to emacs while maintaining both configs for dual-platform use (macOS + Linux). The emacs config uses the older Ivy/Counsel/Swiper completion stack and is missing most of the keybindings established in neovim. Muscle memory consistency across both editors is critical.

## What Changes

- **Remove** Ivy, Counsel, Swiper, Smex, and Projectile packages
- **Add** Vertico, Orderless, Marginalia, Consult, and Embark as the modern completion stack
- **Replace** Projectile with built-in `project.el`
- **Add** Forge (GitHub integration for Magit), consult-eglot, diff-hl, Embark
- **Align** all Evil leader (`SPC`) keybindings to match neovim config
- **Wire** LSP navigation bindings (`gd`, `gr`, `gi`) through eglot
- **Add** Git keybindings matching neovim's fugitive bindings via Magit
- **Add** toggle option bindings (`yo*` prefix) matching neovim's unimpaired-style toggles
- **Map** `-` to `dired-jump` for file browsing (matching Oil.nvim)
- **Conditionalize** macOS-specific settings behind `(eq system-type 'darwin)`
- **Platform-aware** font configuration

## Capabilities

### New Capabilities
- `completion-stack`: Modern Vertico/Consult completion replacing Ivy/Counsel
- `keybinding-parity`: Evil leader bindings aligned with neovim config
- `git-workflow`: Magit keybindings + Forge + diff-hl gutter signs
- `platform-support`: Conditional macOS/Linux configuration

### Modified Capabilities

## Impact

- **File**: `.config/emacs/init.el` — primary file being rewritten
- **Packages removed**: ivy, counsel, swiper, smex, projectile
- **Packages added**: vertico, orderless, marginalia, consult, embark, forge, consult-eglot, diff-hl
- **Dependencies**: Forge requires `ghub` (GitHub API) and sqlite
- **Platform**: Config must work on both macOS and Linux without errors
