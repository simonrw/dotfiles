## 1. Establish the migration baseline

- [x] 1.1 Confirm the first mise release that supports platform `auto_env` discovery and the complete nested `mise bootstrap dotfiles` command set, then document and enforce that minimum version.
- [x] 1.2 Inventory the common, macOS-only, Linux-only, generated, secret, and repository-only paths represented by the current Stow configuration.
- [x] 1.3 Inspect current Stow link shapes and identify targets that mise can adopt versus targets that require an explicit cutover step.

## 2. Declare dotfile ownership

- [x] 2.1 Add early mise configuration that enables automatic platform config environments.
- [x] 2.2 Add explicit common dotfile mappings to `mise.toml`, using symlinks by default and preserving unmanaged neighboring files.
- [x] 2.3 Add macOS-only mappings in `mise.macos.toml` for `Library`, Hammerspoon, and other current macOS targets.
- [x] 2.4 Add Linux-only mappings in `mise.linux.toml` for systemd, fontconfig, desktop portal, Waybar, Hyprland, Sway, and `.local` targets.
- [x] 2.5 Verify that repository support files and ignored generated or secret files have no dotfile declarations.

## 3. Add host-specific templating

- [x] 3.1 Add a Ghostty `local.config` template that selects `.config/ghostty/<short-hostname>.config` and reports the expected path when the profile is missing.
- [x] 3.2 Declare Ghostty's stable config, host profile sources, and rendered target without installing every host profile.
- [x] 3.3 Verify rendered Ghostty output for an existing hostname and the error for a missing hostname profile.

## 4. Switch the installation workflow

- [x] 4.1 Replace the Stow shell logic in the existing `dotfiles` task with `mise bootstrap dotfiles apply` while preserving submodule ordering and the `all` task graph.
- [x] 4.2 Document direct dry-run, apply, status, edit, and unapply commands plus the existing `mise run all` installation path.
- [x] 4.3 Document the live cutover and rollback procedure, including how to handle compatible links, conflicting link shapes, and unmanaged real files without a forced default apply.

## 5. Verify the migration

- [x] 5.1 Add an isolated-home test or verification script that checks common target creation, unmanaged neighbor preservation, and idempotent reapply without touching live dotfiles.
- [x] 5.2 Verify macOS config discovery and confirm that Linux-only targets are absent from the resolved target set.
- [x] 5.3 Verify Linux config discovery and confirm that macOS-only targets are absent from the resolved target set.
- [x] 5.4 Verify dry-run causes no filesystem changes and status with `--missing` detects a missing or drifted target.
- [x] 5.5 Compare the resolved mise targets with the Stow inventory and account for every intentional current target before removing Stow.

## 6. Remove Stow

- [x] 6.1 Remove GNU Stow from `Brewfile` and `ansible/packages.yml` after the isolated and platform checks pass.
- [x] 6.2 Delete `.stow-local-ignore` and confirm no scripts, tasks, documentation, or workflows still invoke Stow.
- [x] 6.3 Run the repository's relevant formatting, validation, and OpenSpec validation commands, then review the final diff for unintended dotfile ownership.
