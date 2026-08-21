# Minimum mise version for platform environments and dotfiles

Use **mise v2026.7.16** as the minimum version.

## Evidence

- mise v2026.6.3 introduced the opt-in `auto_env` setting and automatic platform-derived environments in the order `unix`, `{os}`, `{os}-{arch}`, then explicit `MISE_ENV`. The official release notes call this feature out as added in that release.[1] The tagged implementation builds those platform names and includes them in config discovery only when `auto_env` is enabled.[2]
- v2026.7.15 is too old. Its tagged `BootstrapDotfilesCommands` enum contains only `apply` and `status`.[3]
- v2026.7.16 added nested `add`, `edit`, and `unapply` alongside `apply` and `status`. Its release notes list both the consolidation under `mise bootstrap dotfiles` and the new `unapply` command.[4] The tagged enum contains all five required commands.[5]

The later feature determines the combined minimum, so v2026.7.16 is the first release that supports both requirements.

`auto_env` remains disabled by default in this release. Set `auto_env = true` in `.miserc.toml` or `MISE_AUTO_ENV=true` to activate platform config discovery. The v2026.6.3 release notes say the default will switch on in v2027.6.0.[1]

## Sources

1. [mise v2026.6.3 release notes](https://github.com/jdx/mise/releases/tag/v2026.6.3)
2. [v2026.6.3 platform environment implementation](https://github.com/jdx/mise/blob/v2026.6.3/src/env.rs#L280-L323)
3. [v2026.7.15 nested dotfiles command enum](https://github.com/jdx/mise/blob/v2026.7.15/src/cli/bootstrap.rs#L176-L188)
4. [mise v2026.7.16 release notes](https://github.com/jdx/mise/releases/tag/v2026.7.16)
5. [v2026.7.16 nested dotfiles command enum](https://github.com/jdx/mise/blob/v2026.7.16/src/cli/bootstrap.rs#L180-L195)
