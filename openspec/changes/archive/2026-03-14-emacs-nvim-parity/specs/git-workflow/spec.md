## ADDED Requirements

### Requirement: Magit keybindings
The system SHALL map git operations to Evil leader bindings matching neovim's fugitive bindings.

#### Scenario: Git status
- **WHEN** user presses `gs` in normal mode
- **THEN** `magit-status` opens

#### Scenario: Git commit
- **WHEN** user presses `SPC gc`
- **THEN** `magit-commit` is invoked

#### Scenario: Git diff
- **WHEN** user presses `SPC gd`
- **THEN** `magit-diff-dwim` is invoked

#### Scenario: Git stage file
- **WHEN** user presses `SPC gw`
- **THEN** current file is staged via `magit-stage-file`

#### Scenario: Git unstage file
- **WHEN** user presses `SPC gr`
- **THEN** current file is unstaged via `magit-unstage-file`

#### Scenario: Git amend
- **WHEN** user presses `SPC ga`
- **THEN** `magit-commit-amend` is invoked

### Requirement: Git gutter signs
The system SHALL show git diff indicators in the gutter via diff-hl.

#### Scenario: Modified lines shown
- **WHEN** a file with uncommitted changes is opened
- **THEN** added, modified, and deleted lines are indicated in the gutter

#### Scenario: Hunk navigation
- **WHEN** user presses `]c` in normal mode
- **THEN** cursor moves to the next changed hunk

#### Scenario: Previous hunk
- **WHEN** user presses `[c` in normal mode
- **THEN** cursor moves to the previous changed hunk

### Requirement: Forge integration
The system SHALL include Forge for GitHub PR/issue management within Magit.

#### Scenario: Forge loaded with Magit
- **WHEN** Magit is loaded
- **THEN** Forge is available for GitHub operations
