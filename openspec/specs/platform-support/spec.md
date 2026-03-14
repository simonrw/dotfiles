## ADDED Requirements

### Requirement: macOS-specific settings conditionalized
The system SHALL only apply macOS-specific settings when running on macOS.

#### Scenario: macOS key remapping
- **WHEN** Emacs runs on macOS
- **THEN** ns-command-modifier, ns-option-modifier, and related settings are applied

#### Scenario: Linux startup
- **WHEN** Emacs runs on Linux
- **THEN** no macOS-specific settings (ns-*, mdfind, transparent titlebar) are applied

### Requirement: Platform-aware fonts
The system SHALL configure fonts appropriate to the current platform.

#### Scenario: Font configuration
- **WHEN** Emacs starts on a graphical display
- **THEN** fonts are set with platform-appropriate faces and sizes

### Requirement: Shell environment import
The system SHALL import shell environment variables on macOS where GUI apps don't inherit shell env.

#### Scenario: macOS shell env
- **WHEN** Emacs runs on macOS
- **THEN** exec-path-from-shell imports PATH and other variables

#### Scenario: Linux shell env
- **WHEN** Emacs runs on Linux
- **THEN** exec-path-from-shell is not needed (terminal Emacs inherits env)
