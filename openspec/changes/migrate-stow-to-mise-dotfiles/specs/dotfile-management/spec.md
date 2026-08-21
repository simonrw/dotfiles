## ADDED Requirements

### Requirement: Explicit dotfile ownership
The system SHALL manage dotfiles through explicit mise target-to-source declarations and SHALL leave repository files without a declaration unmanaged.

#### Scenario: Apply common dotfiles
- **WHEN** a user applies dotfiles from a supported host
- **THEN** mise creates the declared common targets from their repository sources

#### Scenario: Ignore repository support files
- **WHEN** the repository contains provisioning code, documentation, generated output, or OpenSpec artifacts without dotfile declarations
- **THEN** mise does not create corresponding paths in the user's home directory

#### Scenario: Preserve unmanaged neighbors
- **WHEN** a managed target directory contains a file not owned by mise
- **THEN** applying dotfiles leaves that file unchanged

### Requirement: Platform-specific target selection
The system SHALL load common mappings on every supported platform and SHALL automatically add only the mappings for the current operating system.

#### Scenario: Install on macOS
- **WHEN** dotfiles are applied on macOS
- **THEN** mise manages common and macOS targets and does not manage Linux-only targets

#### Scenario: Install on Linux
- **WHEN** dotfiles are applied on Linux
- **THEN** mise manages common and Linux targets and does not manage macOS-only targets

### Requirement: Symlink-first behavior
The system SHALL use symlinks for ordinary dotfiles and directories unless a declared target requires rendered or copied content.

#### Scenario: Edit a symlinked source
- **WHEN** a user edits the repository source for an ordinary managed dotfile
- **THEN** the target reflects the edit without another apply operation

### Requirement: Host-specific Ghostty rendering
The system SHALL render Ghostty's `local.config` from the repository profile matching the host's short hostname.

#### Scenario: Matching host profile exists
- **WHEN** dotfiles are applied and `.config/ghostty/<short-hostname>.config` exists
- **THEN** `~/.config/ghostty/local.config` contains the selected profile's content as a regular rendered file

#### Scenario: Matching host profile is absent
- **WHEN** dotfiles are applied and the expected hostname profile does not exist
- **THEN** mise fails the render with an error that identifies the missing profile

### Requirement: Observable and idempotent application
The system SHALL provide dry-run, apply, and status commands through the supported nested mise dotfile command set.

#### Scenario: Preview changes
- **WHEN** a user runs the dotfile apply command with `--dry-run`
- **THEN** mise reports planned filesystem changes without changing the home directory

#### Scenario: Reapply desired state
- **WHEN** a user applies dotfiles twice without changing sources or targets
- **THEN** the second apply reports no required filesystem changes

#### Scenario: Detect drift
- **WHEN** a declared target is missing or differs from its desired rendered or copied content
- **THEN** dotfile status with `--missing` reports the target and exits unsuccessfully

### Requirement: Stow-free installation
The system SHALL install the managed dotfiles without requiring GNU Stow.

#### Scenario: Fresh machine installation
- **WHEN** a machine has Git, a supported mise release, and a clone of the repository
- **THEN** the repository's dotfile installation workflow can apply all applicable mappings without invoking Stow

#### Scenario: Package provisioning
- **WHEN** Homebrew or Ansible installs the repository's declared packages after migration
- **THEN** GNU Stow is not included solely for dotfile management

### Requirement: Non-destructive cutover
The migration SHALL detect conflicting existing targets and SHALL NOT overwrite unmanaged real files during the default apply workflow.

#### Scenario: Existing compatible link
- **WHEN** a Stow-created symlink already points to the source declared by mise
- **THEN** the migration preserves or adopts the compatible link without deleting its source

#### Scenario: Existing incompatible target
- **WHEN** a target has a conflicting link shape or is an unmanaged real file
- **THEN** the default apply stops and reports the conflict without using force

#### Scenario: Isolated verification
- **WHEN** automated migration checks run
- **THEN** they use an isolated home directory and do not alter the operator's live dotfiles
