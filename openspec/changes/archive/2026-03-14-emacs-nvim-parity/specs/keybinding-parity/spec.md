## ADDED Requirements

### Requirement: Evil leader general bindings
The system SHALL map these Evil leader bindings to match neovim.

#### Scenario: Save file
- **WHEN** user presses `SPC w`
- **THEN** current buffer is saved

#### Scenario: Quit
- **WHEN** user presses `SPC q`
- **THEN** Emacs quits (save-buffers-kill-terminal)

#### Scenario: Reload config
- **WHEN** user presses `SPC o`
- **THEN** init.el is reloaded

#### Scenario: Copy file to clipboard
- **WHEN** user presses `cp` in normal mode
- **THEN** entire buffer contents are copied to system clipboard

### Requirement: LSP keybindings
The system SHALL map LSP navigation and action bindings in eglot-managed buffers.

#### Scenario: Go to definition
- **WHEN** user presses `gd` in an eglot buffer
- **THEN** `xref-find-definitions` is invoked

#### Scenario: Go to references
- **WHEN** user presses `gr` in an eglot buffer
- **THEN** `xref-find-references` is invoked

#### Scenario: Go to implementation
- **WHEN** user presses `gi` in an eglot buffer
- **THEN** `eglot-find-implementation` is invoked

#### Scenario: Go to type definition
- **WHEN** user presses `SPC gt` in an eglot buffer
- **THEN** `eglot-find-typeDefinition` is invoked

#### Scenario: Rename symbol
- **WHEN** user presses `SPC r`
- **THEN** `eglot-rename` is invoked

#### Scenario: Format buffer
- **WHEN** user presses `SPC y`
- **THEN** `eglot-format` is invoked

#### Scenario: Code actions
- **WHEN** user presses `SPC a`
- **THEN** `eglot-code-actions` is invoked

#### Scenario: Buffer diagnostics
- **WHEN** user presses `SPC d`
- **THEN** `flymake-show-buffer-diagnostics` is shown

#### Scenario: Project diagnostics
- **WHEN** user presses `SPC A`
- **THEN** `flymake-show-project-diagnostics` is shown

#### Scenario: Document symbols
- **WHEN** user presses `SPC S`
- **THEN** `consult-imenu` shows document symbols

#### Scenario: Workspace symbols
- **WHEN** user presses `SPC s`
- **THEN** `consult-eglot-symbols` shows workspace symbols

### Requirement: Toggle option bindings
The system SHALL provide yo-prefixed toggle bindings matching neovim.

#### Scenario: Toggle word wrap
- **WHEN** user presses `yow`
- **THEN** `visual-line-mode` is toggled

#### Scenario: Toggle line numbers
- **WHEN** user presses `yon`
- **THEN** `display-line-numbers-mode` is toggled

#### Scenario: Toggle relative line numbers
- **WHEN** user presses `yor`
- **THEN** line numbers switch between absolute and relative

#### Scenario: Toggle spell checking
- **WHEN** user presses `yos`
- **THEN** `flyspell-mode` is toggled

#### Scenario: Toggle zen mode
- **WHEN** user presses `yoz`
- **THEN** `visual-fill-column-mode` is toggled

#### Scenario: Toggle git blame
- **WHEN** user presses `yob`
- **THEN** `magit-blame` is toggled

### Requirement: File browsing
The system SHALL map `-` to dired-jump for file browsing.

#### Scenario: Open file browser
- **WHEN** user presses `-` in normal mode
- **THEN** dired opens at the current file's directory

### Requirement: Quickfix toggle
The system SHALL map `Q` to toggle the flymake diagnostics buffer.

#### Scenario: Toggle diagnostics
- **WHEN** user presses `Q` in normal mode
- **THEN** the flymake diagnostics buffer is toggled
