# Dotfile management

mise 2026.7.16 or newer manages the dotfiles. This is the first release with platform environments and the nested `add`, `apply`, `edit`, `status`, and `unapply` commands. `.miserc.toml` enables platform environment discovery, which loads `mise.macos.toml` on macOS or `mise.linux.toml` on Linux in addition to `mise.toml`. See [the version research](research/mise-dotfiles-minimum-version.md) for the release evidence.

## Commands

Run these from the repository root:

```sh
mise bootstrap dotfiles apply --dry-run
mise bootstrap dotfiles apply
mise bootstrap dotfiles status
mise bootstrap dotfiles status --missing
mise bootstrap dotfiles edit ~/.zshrc
mise bootstrap dotfiles unapply
```

`mise run all` remains the full installation path. Its `dotfiles` dependency initializes submodules before it applies the mappings.

Do not use `--force` during a normal apply or cutover. A conflict should stop the command so the existing target can be inspected.

## Ownership inventory

The resolved inventory has 62 common declarations, 10 macOS declarations, and 7 Linux declarations.

Common ownership includes shell entry points, `.bin`, selected Claude and Codex files, editor and terminal configuration, Git and SSH configuration, and explicitly selected children of `.config`. Directories with runtime neighbors use child declarations. Examples include Fish without `fish_variables`, Herdr without session files and logs, jj without `repos`, and Zed without `settings.json` or `prompts`.

macOS adds Karabiner, Hammerspoon, the Pi macOS theme extension, VS Code and Cursor settings, Firefox, claude-session-monitor, and sapling paths below `Library`.

Linux adds fontconfig, Hyprland, Sway, systemd, Waybar, desktop portal configuration, and the Helium desktop entry below `.local`.

Ghostty's stable `config` is a symlink. The named `*.config` host profiles remain repository sources and are not installed individually. mise renders `local.config` as a regular file from the profile matching `hostname -s`.

The declarations exclude generated, secret, and machine-local files such as `.config/gh/hosts.yml`, `.codex/config.toml`, `.codex/environments`, `.config/fish/fish_variables`, `.config/helix/themes/adaptive.toml`, Herdr runtime state, jj repository state, Zed settings and prompts, `.claude/worktrees`, `.junie`, `.jj`, `.ruff_cache`, `mise.local.toml`, and `scratch`.

Repository support paths also have no declarations. This includes `.git`, `.github`, `AGENTS.md`, `ansible`, Brewfiles, `cmd`, `docs`, `gh-extensions`, Go module files, `internal`, `mise-tasks`, OpenSpec artifacts, `raycast`, and `theming`.

## Existing Stow link shapes

The macOS inventory taken on 2026-08-21 found 54 targets that mise already recognizes as applied, 17 targets whose Stow link shape or real-file content conflicts with the declaration, and one new missing target.

The targets needing an explicit cutover are:

- The real `~/.config/atuin/config.toml`.
- Folded Stow directories for Fish, Ghostty, Herdr, jj, and Zed. These must become real parent directories containing only the declared links or rendered file.
- Nested directory shapes for `~/.config/bat/themes` and `~/.config/opencode`.
- The Pi `extensions` directory, which must become a real directory containing the two platform-selected links.
- Cursor's real `keybindings.json`.

`~/.pi/agent/AGENTS.md` is the missing declaration. The exact live list is available from `mise bootstrap dotfiles status` before cutover.

## Live cutover

1. Upgrade mise to 2026.7.16 or newer and run `mise bootstrap dotfiles apply --dry-run`.
2. Run `mise bootstrap dotfiles status`. Leave entries reported as applied alone. They are compatible Stow links that mise can adopt.
3. For each entry reported as different, inspect it with `ls -ld` and `readlink`. If it is a Stow symlink, unlink only that symlink. If a folded directory must be expanded, unlink the directory link, create the parent directory, and let mise add the declared children.
4. If the target is a real file, compare it with the repository source. Move it to a backup outside the target path if it contains local changes. Never replace it with `--force`.
5. Run `mise bootstrap dotfiles apply` without `--force`, then `mise bootstrap dotfiles status --missing`.
6. Run the higher-level configuration with `mise run all` when the dotfile status is clean.

The Ghostty cutover replaces the old `local.config` symlink with rendered content. Review that link before removing it. The host profile remains in the repository.

## Rollback

Review rendered or copied targets for local edits before removing them. Run `mise bootstrap dotfiles unapply`, restore `.stow-local-ignore`, the old `dotfiles` task, and the Stow package declarations from Git, reinstall Stow, then run the restored `mise run dotfiles`. Keep any backed-up real files until the restored setup has been checked.

## Verification

`scripts/verify-dotfiles` uses a temporary home directory. It checks platform selection, target counts, ignored paths, dry-run behavior, unmanaged neighbors, apply, rendered Ghostty content, idempotent reapply, missing-target status, and the missing-host-profile error. It never applies mappings to the live home directory.
