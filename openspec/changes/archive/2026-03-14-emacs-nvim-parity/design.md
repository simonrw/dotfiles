## Context

The dotfiles repo contains both neovim (`.config/nvim/init.lua`) and emacs (`.config/emacs/init.el`) configurations. The neovim config is mature with consistent keybindings, LSP integration, and git workflow. The emacs config uses Evil mode with Space as leader but has an older completion stack (Ivy/Counsel/Swiper) and is missing most of the keybindings established in neovim. The config also has macOS-specific settings that need to be conditionalized for Linux.

## Goals / Non-Goals

**Goals:**
- Consistent keybindings across neovim and emacs for muscle memory
- Modern completion stack (Vertico ecosystem)
- Working config on both macOS and Linux
- Git workflow parity via Magit bindings + Forge + diff-hl

**Non-Goals:**
- DAP / debugger setup
- Test runner integration
- Porting Octo.nvim (Forge covers GitHub integration)
- Changing the neovim config

## Decisions

### 1. Vertico + Orderless + Marginalia + Consult + Embark over keeping Ivy

**Rationale**: This is the actively-maintained modern stack. Consult integrates natively with `project.el` and `xref`. Ivy/Counsel development has slowed. The Vertico ecosystem is more composable — each piece does one thing.

**Alternative considered**: Keep Ivy and just add Consult on top. Rejected because Ivy and Vertico conflict as completion UIs, and maintaining both adds complexity.

### 2. project.el over Projectile

**Rationale**: Built-in, zero-config, native Consult integration via `consult-project-buffer`. Eglot already uses `project.el` internally. Fewer packages to maintain.

**Alternative considered**: Keep Projectile. It has more features (search paths, caching), but `project.el` covers the core need (find-file, switch-buffer, grep within project) and is what the ecosystem is converging on.

### 3. diff-hl for git gutter signs

**Rationale**: Lightweight, well-maintained, integrates with Magit. Closest equivalent to gitsigns.nvim. Provides `]c`/`[c` hunk navigation matching neovim.

### 4. Evil leader bindings via evil-leader

**Rationale**: Already in the config. Keep using it rather than switching to `general.el` — simpler, already works, less churn.

### 5. Single init.el approach

**Rationale**: Keep all config in one `init.el` rather than splitting into modules. Matches the current approach in both editors and keeps things simple for a dotfiles repo.

## Risks / Trade-offs

- **[Evil-collection conflicts]** → Some evil-collection bindings may shadow our custom `gd`/`gr`/`gi` in specific modes. Mitigation: Set bindings in `eglot-mode-map` with higher priority via `evil-define-key`.
- **[Forge requires sqlite]** → Forge uses an emacsql-sqlite backend. Mitigation: Emacs 29+ includes native sqlite support; ensure `sqlite3` is available on both platforms.
- **[project.el less featureful]** → No equivalent to Projectile's `projectile-search-path` for auto-discovering projects. Mitigation: Use `project-remember-projects-under` or just open projects manually — acceptable for a personal setup.
