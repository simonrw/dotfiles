# mise-managed binaries in launchd agents

## Recommendation

Use each tool's stable mise shim as the launchd `program`, and set the working directory to this repo so mise loads `mise.toml` and `mise.macos.toml`:

```toml
[tools]
node = "lts"
"npm:@jmfederico/pi-web" = "latest"

[bootstrap.macos.launchd.agents.pi-web-sessiond]
program = "~/.local/share/mise/shims/pi-web-sessiond"
working_directory = "~/dotfiles"
environment = { MISE_ENABLE_TOOLS = "node,npm:@jmfederico/pi-web" }
run_at_load = true
keep_alive = true
stdout_path = "~/Library/Logs/pi-web-sessiond.log"
stderr_path = "~/Library/Logs/pi-web-sessiond.log"

[bootstrap.macos.launchd.agents.pi-web-server]
program = "~/.local/share/mise/shims/pi-web-server"
working_directory = "~/dotfiles"
environment = { MISE_ENABLE_TOOLS = "node,npm:@jmfederico/pi-web", PI_WEB_HOST = "0.0.0.0" }
run_at_load = true
keep_alive = true
stdout_path = "~/Library/Logs/pi-web-server.log"
stderr_path = "~/Library/Logs/pi-web-server.log"
```

This removes the package layout from the service definitions and removes the shell wrapper. mise creates one shim per installed executable in `~/.local/share/mise/shims`; a shim delegates back to mise, which resolves the active tool from the current configuration. mise also refreshes shims when tools are installed, updated, or removed.[1]

Pi Web's launcher calls `node`, so Node must be active alongside the npm package. `MISE_ENABLE_TOOLS` limits each daemon to those two tools.[8] Without the allowlist, the shim resolves every active tool in the repo and global configuration before starting Pi Web. On this machine that made both agents wait on Rust tool probing instead of opening their socket and HTTP listener.

`working_directory` is required with the present configuration. `npm:@jmfederico/pi-web` is declared in `mise.macos.toml`, and running `mise which pi-web-server` outside this repo reports that the binary is not active. launchd changes to the configured directory before starting the program.[2]

Apply the definitions with:

```sh
mise bootstrap macos launchd-agents apply --yes
```

mise writes `~/Library/LaunchAgents/dev.mise.<name>.plist`, unloads and rewrites changed definitions, then loads and enables them.[3]

## Why this works

The launchd bootstrap schema does not have a special "mise tool" field. `program` becomes `ProgramArguments[0]`, `args` become the remaining argument vector, and launchd executes that vector directly.[2][3] The mise renderer expands `~` in `program` and `working_directory`, but passes `args` through unchanged.[3] It does not render launchd fields as Tera templates, so values such as `program = "{{ mise_bin }}"` or `program = "{{ tools...path }}"` remain literal strings.[5]

The shim is better than resolving `mise which pi-web-server` while generating the plist. A resolved install path would freeze the current version in the plist and require another apply after every upgrade. The shim resolves the active version on every process start.

The current hard-coded `latest/node_modules/.bin/...` path partly avoids version numbers, but it depends on the npm backend's private install layout. The shim is mise's public indirection for this job.[1]

## Upgrade lifecycle

Upgrading `pi-web` needs no plist rewrite. mise refreshes shims during tool updates.[1] A process that is already running continues to run the old executable until it exits. Restart both agents after an upgrade so their shims resolve the new version:

```sh
launchctl kickstart -k "gui/$(id -u)/dev.mise.pi-web-sessiond"
launchctl kickstart -k "gui/$(id -u)/dev.mise.pi-web-server"
```

`keep_alive = true` also causes launchd to start the service again after a later exit.[4] Setting `kickstart = true` does not make ordinary tool upgrades restart an unchanged agent. mise only applies missing or changed definitions, and calls `launchctl kickstart -k` while applying a selected definition.[3][5]

## Other viable forms

Calling `mise exec` directly gives the same dynamic resolution and the full mise environment:

```toml
program = "/opt/homebrew/bin/mise"
args = ["exec", "--", "pi-web-server"]
working_directory = "~/dotfiles"
```

`mise exec` loads tools from the active `mise.toml` hierarchy and places their executables on the child process environment.[6] This form hard-codes only mise's Homebrew path, not the tool install. It is useful if shim behavior ever lacks a needed mise feature, although shims do pass mise environment variables to the managed tool.[1]

A path-independent mise invocation can use `/usr/bin/env` with an explicit PATH:

```toml
program = "/usr/bin/env"
args = ["mise", "exec", "--", "pi-web-server"]
working_directory = "~/dotfiles"
environment = { PATH = "/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin" }
```

This is more moving parts than the shim and still fixes the Homebrew directory in `PATH`.

The existing `/bin/zsh -lc` form can run `exec mise exec -- pi-web-server` because a login zsh reads `.zprofile`, and this repo's `.zprofile` adds `/opt/homebrew/bin`. It does not read `.zshrc` unless the shell is interactive, so the mise activation in `.config/zsh/mise.zsh` is not involved.[7] Calling the tool name directly from `zsh -lc` is therefore unreliable unless the command uses a shim path or `mise exec`. The shell adds no value for these two agents.

## Sources

1. [mise v2026.8.10 shim documentation](https://github.com/jdx/mise/blob/v2026.8.10/docs/dev-tools/shims.md#mise-activate-shims)
2. [Apple launchd.plist source: `ProgramArguments`, `WorkingDirectory`, and `EnvironmentVariables`](https://github.com/apple-oss-distributions/launchd/blob/d448a1c8f70a61202f8705f94337f686b87c30c4/man/launchd.plist.5#L145-L221)
3. [mise v2026.8.10 launchd documentation](https://github.com/jdx/mise/blob/v2026.8.10/docs/bootstrap/launchd.md)
4. [Apple launchd.plist source: `KeepAlive`](https://github.com/apple-oss-distributions/launchd/blob/d448a1c8f70a61202f8705f94337f686b87c30c4/man/launchd.plist.5#L175-L187)
5. [mise v2026.8.10 launchd renderer and apply implementation](https://github.com/jdx/mise/blob/v2026.8.10/src/system/launchd.rs#L251-L380)
6. [mise v2026.8.10 `mise exec` documentation](https://github.com/jdx/mise/blob/v2026.8.10/docs/cli/exec.md)
7. [zsh startup and shutdown files](https://zsh.sourceforge.io/Doc/Release/Files.html#Startup_002fShutdown-Files)
8. [mise v2026.8.10 `enable_tools` setting](https://github.com/jdx/mise/blob/v2026.8.10/settings.toml#L596-L605)
