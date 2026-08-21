## Context

The repository is both a dotfile source tree and a collection of provisioning code, generated configuration, OpenSpec artifacts, and helper projects. GNU Stow currently distinguishes those roles through `.stow-local-ignore`, then the `dotfiles` task adds separate ignore expressions for macOS and Linux. Stow may fold an entire source directory into one symlink when the target does not exist, so existing machines contain a mixture of file and directory links.

mise 2026.8 adds the dotfile command set needed for this design: declarative target mappings, `symlink`, `symlink-each`, `copy`, and `template` modes, platform config environments, status reporting, dry runs, and unapply support. The repository's installed mise 2026.7.5 is older, so the migration must establish and check a minimum supported version before relying on the current command set.

The existing `mise run all` task is the public installation path. This change replaces its Stow implementation without redesigning the rest of the provisioning graph.

## Goals / Non-Goals

**Goals:**

- Make mise the only dotfile installer.
- Declare common and platform-specific ownership without a repository-wide negative ignore list.
- Preserve the current target paths and live-edit behavior for ordinary symlinked configuration.
- Use mise templating for host-dependent Ghostty configuration.
- Provide repeatable apply, dry-run, status, and isolated verification workflows.
- Cut over without overwriting unmanaged user files.

**Non-Goals:**

- Replace Homebrew, Ansible, or the existing `mise run all` orchestration with the broader `mise bootstrap` package system.
- Template files that do not vary by platform or host.
- Move dotfile sources into a new repository layout solely to match mise conventions.
- Manage secrets or previously ignored generated files.
- Apply the migration to a live home directory as part of automated tests.

## Decisions

### Use explicit grouped mappings

Declare whole-file and whole-directory targets where the repository owns the complete target. Use `symlink-each` only when a target directory must coexist with unmanaged files. Keep sources relative to the repository's mise config.

This is more verbose than mapping the repository root to `~` with `symlink-each`, but it removes the need to restate the current long ignore list. It also makes accidental additions such as `openspec`, `cmd`, or `AGENTS.md` harmless until explicitly managed.

The root-level `symlink-each` alternative was rejected because it preserves Stow's implicit ownership model and creates a broad target whose exclusions remain easy to get wrong.

### Separate platform ownership through mise config environments

Keep common mappings in `mise.toml`. Put macOS-only mappings in `mise.macos.toml` and Linux-only mappings in `mise.linux.toml`. Enable mise platform environment discovery through `.miserc.toml` so the correct file loads without shell conditionals.

This replaces the `uname` branch in the current task and lets `mise config` show the platform-specific source of each declaration. Calling mise with `-E macos` or `-E linux` was considered, but it would retain platform selection logic in the task and make direct dotfile commands easier to misuse.

### Keep symlinks as the default mode

Use symlinks for ordinary files and directories so editing a repository source updates the live configuration immediately. Use `template` only for generated output and `copy` only if a target program requires a regular writable file.

This preserves the useful part of the Stow workflow. A copy-first design would require users to recapture routine edits and would make repository state less obvious.

### Render Ghostty's local configuration

Manage Ghostty's stable `config` as a symlink. Keep the named host profiles as source data rather than installing each profile. Render `~/.config/ghostty/local.config` from the profile selected by the short hostname.

The template may use a side-effect-free `hostname -s` call and `read_file()` to select the profile. Applying or checking status MUST fail clearly when no matching profile exists. A machine-local hostname variable was considered, but it adds a setup step and another untracked file for information the operating system already provides.

### Retain `mise run all` during this change

Change the existing `dotfiles` task to invoke `mise bootstrap dotfiles apply`, while preserving its dependency on submodule initialization and its place in the `all` task graph. Users may run the nested dotfile status and apply commands directly.

Making `mise bootstrap` the sole top-level installer could simplify the repository later, but that also changes package provisioning, task ordering, and final hooks. It should be evaluated separately after this cutover.

### Require a current mise command set

Document and enforce a minimum mise version that includes nested add, apply, status, edit, and unapply commands plus platform config environments. Do not build the workflow around the deprecated top-level `mise dotfiles` command.

## Risks / Trade-offs

- [Existing Stow links have a different shape] -> Prefer mappings that match existing directory ownership, inventory link shapes before cutover, and un-stow only mappings that mise cannot adopt.
- [A broad mapping captures generated or secret files] -> Use positive target declarations and test the resolved target list against the current intended Stow set.
- [Platform config loads incorrectly] -> Pin the minimum mise version and verify `mise config` on isolated macOS and Linux runs.
- [Ghostty has no profile for a new hostname] -> Make template rendering fail with a message naming the expected profile path.
- [Template status executes `hostname`] -> Keep the command side-effect-free and avoid network or credential access in templates.
- [Removing Stow makes rollback less immediate] -> Remove the package dependency and ignore file only after mise verification; Git can restore them if rollback is needed.
- [Explicit mappings require maintenance] -> Treat that verbosity as the ownership boundary. New dotfiles must be intentionally added rather than appearing in `$HOME` because they were committed near other files.

## Migration Plan

1. Establish the supported mise version and add a preflight check.
2. Record the current intended common, macOS, and Linux target sets from the Stow configuration.
3. Add common and platform-specific mise declarations plus the Ghostty template.
4. Verify config discovery, target selection, template output, status, and idempotent apply using isolated temporary home directories.
5. Compare a verbose mise dry run with the current Stow-managed targets on representative macOS and Linux hosts.
6. For a live cutover, remove only Stow-managed links that conflict with the desired mise link shape, then apply mise dotfiles without `--force`.
7. Run status with `--missing`, followed by the existing higher-level configuration tasks.
8. Remove the Stow task body, `.stow-local-ignore`, and Stow package declarations after both platform checks pass.

Rollback restores the Stow files and package declarations from Git, unapplies identifiable mise targets, and reruns Stow. Any rendered or copied target that has been edited since apply must be reviewed before removal.

## Open Questions

- Which exact mise release first contains both platform `auto_env` discovery and the complete nested dotfile command set? Implementation must confirm this and set the minimum version accordingly.
- Do any currently folded Stow directories contain machine-local files that rely on being written into an ignored path inside the repository? The inventory task must identify these before choosing whole-directory mappings.
