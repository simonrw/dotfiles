## ADDED Requirements

### Requirement: Vertico completion UI
The system SHALL use Vertico as the minibuffer completion UI, replacing Ivy.

#### Scenario: Minibuffer completion uses Vertico
- **WHEN** any completing-read command is invoked
- **THEN** Vertico provides vertical completion candidates in the minibuffer

### Requirement: Orderless matching
The system SHALL use Orderless for flexible, space-separated matching in completions.

#### Scenario: Space-separated matching
- **WHEN** user types "init lua" in a file finder
- **THEN** candidates matching both "init" and "lua" in any order are shown

### Requirement: Marginalia annotations
The system SHALL use Marginalia to annotate completion candidates with metadata.

#### Scenario: File candidates show metadata
- **WHEN** user browses file candidates
- **THEN** each candidate shows size, date, or other relevant annotations

### Requirement: Consult commands
The system SHALL use Consult for enhanced search and navigation commands.

#### Scenario: Project-wide grep
- **WHEN** user invokes `SPC SPC`
- **THEN** `consult-ripgrep` runs against the current project root

#### Scenario: Buffer switching
- **WHEN** user invokes `gb` in evil normal mode
- **THEN** `consult-buffer` shows buffers with preview

#### Scenario: Find file
- **WHEN** user invokes `SPC F`
- **THEN** `consult-find` searches for files

### Requirement: Embark contextual actions
The system SHALL use Embark for contextual actions on completion candidates.

#### Scenario: Act on candidate
- **WHEN** user invokes Embark on a completion candidate
- **THEN** a context menu of relevant actions is shown

### Requirement: Remove old completion packages
The system SHALL NOT include Ivy, Counsel, Swiper, Smex, or Projectile.

#### Scenario: No Ivy packages loaded
- **WHEN** Emacs starts
- **THEN** none of ivy, counsel, swiper, smex, or projectile are loaded

### Requirement: project.el for project management
The system SHALL use built-in `project.el` instead of Projectile.

#### Scenario: Find file in project
- **WHEN** user invokes `SPC f`
- **THEN** `project-find-file` searches within the current project
