# Shared SSH agent on macOS

Each Mac uses its own Apple-managed SSH agent. GUI apps and shells on that Mac
share it. No agent or keys are forwarded from the machine initiating SSH;
`ForwardAgent no` remains the default in `.ssh/config`.

## How it works

Apple's `com.openssh.ssh-agent` LaunchAgent supplies `SSH_AUTH_SOCK` to the GUI
login session. The `ssh-agent-socket` LaunchAgent publishes that socket as
`~/.ssh/macos-agent.sock`, a symlink inside the private `~/.ssh` directory.
It runs at GUI login and retries on failure. It does not start an agent, load
keys, or alter launchd's environment.

`.zshenv` selects this link for every normal zsh invocation, including
non-interactive `ssh mm command` calls. Fish selects it too instead of starting
another agent on macOS. GUI apps keep Apple's original socket path; both paths
connect to the same agent. A valid published local socket takes precedence over
an inherited socket, including an explicitly forwarded agent.

Tmux imports `SSH_AUTH_SOCK` when a client attaches. New zsh panes also read
`.zshenv`, so a stale tmux environment cannot override the published socket.
Existing shells using the stable path follow its updated target after a new GUI
login. No shell starts or stops the macOS agent. Linux agent handling is unchanged.

## Install

With this checkout on each Mac, run from `~/dotfiles`:

```sh
mise bootstrap dotfiles apply ~/.zshenv
mise bootstrap macos launchd apply --dry-run
mise bootstrap macos launchd apply
```

The existing `.bin`, zsh, and Fish mappings must already be installed. If a real
`~/.zshenv` exists, inspect it and preserve its contents before replacing it with
the managed symlink. Do not use `--force` to overwrite it blindly.

Apply launchd configuration while your GUI login session exists. The publisher
must run in that GUI session, not directly from an SSH shell. mise loads it into
`gui/$(id -u)` even when applying over SSH.

For an already-running zsh shell, including an existing tmux pane:

```sh
source ~/.zshenv
```

Detach and reattach tmux from that shell to refresh its session environment.
Already-running programs cannot have their environment changed externally;
restart programs that retained an old socket. GUI apps launched normally should
already have Apple's socket.

## Keys and security

Each host needs its own authorized keys. Prefer a separate key per host rather
than copying your laptop's private key. To load a chosen key into the shared
agent and store its passphrase in the macOS Keychain:

```sh
/usr/bin/ssh-add --apple-use-keychain ~/.ssh/id_ed25519
```

After an agent restart, load keys whose passphrases are already in the unlocked
Keychain with `/usr/bin/ssh-add --apple-load-keychain`. These are Apple's commands;
Homebrew OpenSSH does not provide Apple's Keychain options. Keychain unlocking
may require a GUI login. This setup deliberately does not load every key or
change `AddKeysToAgent no`.

The agent survives SSH disconnects and tmux restarts because launchd owns it.
It depends on a GUI login and is not a headless boot service. Before GUI login,
or after GUI logout, the published link may be missing or stale. No fallback
agent is silently started. Processes running as your user can request signatures
from this agent, just as GUI applications can.

## Verify

```sh
ssh -a mm 'echo "$SSH_AUTH_SOCK"; readlink ~/.ssh/macos-agent.sock; /usr/bin/ssh-add -l'
```

`SSH_AUTH_SOCK` should be `/Users/simon/.ssh/macos-agent.sock`. In a GUI terminal,
compare the identities returned through the original and published paths:

```sh
SSH_AUTH_SOCK="$(launchctl getenv SSH_AUTH_SOCK)" /usr/bin/ssh-add -l
SSH_AUTH_SOCK="$HOME/.ssh/macos-agent.sock" /usr/bin/ssh-add -l
```

An empty `launchctl getenv SSH_AUTH_SOCK` over SSH does **not** mean GUI apps lack
an agent. That command queries the caller's launchd context, not the GUI login
context. For diagnosis over SSH:

```sh
launchctl print gui/$(id -u)/com.openssh.ssh-agent
launchctl print gui/$(id -u)/dev.mise.ssh-agent-socket
```

Check `~/Library/Logs/ssh-agent-socket.log` if publication fails. After fixing the
cause, retry with `launchctl kickstart gui/$(id -u)/dev.mise.ssh-agent-socket`.
The launchd service should exit successfully after publishing, rather than stay
running. `ssh-add -l` exits 1 for a reachable agent with no keys and 2 when it
cannot connect.

Run the isolated integration tests with `python3 tests/macos-ssh-agent.py`.

## Rollback

Boot out `gui/$(id -u)/dev.mise.ssh-agent-socket`, remove its plist from
`~/Library/LaunchAgents`, and revert the tracked configuration changes. Remove
the `~/.zshenv` symlink if this setup created it, restoring any previous file.
Remove `~/.ssh/macos-agent.sock` only if it is the publisher's symlink. New GUI
terminals will still inherit Apple's original agent socket. Apple's service and
keys are not changed by this setup.
