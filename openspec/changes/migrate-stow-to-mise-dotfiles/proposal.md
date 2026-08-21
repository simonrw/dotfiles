## Why

Dotfile installation currently depends on GNU Stow, a repository-wide ignore list, and platform-specific shell exclusions. mise now provides native dotfile management and templating, so the repository can remove one bootstrap dependency and declare exactly which files it owns.

## What Changes

- Replace the Stow-based `dotfiles` task with mise-managed dotfile declarations.
- Split common, macOS-only, and Linux-only dotfile ownership so each platform installs only applicable configuration.
- Render host-dependent configuration through mise templates, beginning with Ghostty's `local.config`.
- Add dry-run and status checks for the managed dotfile set.
- Remove Stow from Homebrew and Ansible package installation after the mise cutover is verified.
- Remove `.stow-local-ignore` and the platform-specific Stow command.
- **BREAKING**: A recent mise release becomes the only supported dotfile installer. Existing installations must remove Stow-managed links before applying mappings whose link structure changes.

## Capabilities

### New Capabilities

- `dotfile-management`: Declarative installation, platform selection, templating, status reporting, and safe migration of user dotfiles through mise.

### Modified Capabilities

None.

## Impact

- Affects `mise.toml`, new platform-specific mise configuration, the dotfile installation task, and the Ghostty host configuration flow.
- Removes `.stow-local-ignore` and the `stow` entries from `Brewfile` and `ansible/packages.yml`.
- Changes the fresh-machine prerequisite set from Git, mise, and Stow to Git and mise.
- Requires validation on macOS and Linux because the current Stow exclusions differ by platform.
