# Event-driven macOS theme updates

## Question

Can a launchd job respond to system theme changes instead of polling, while keeping `reload-theme` as the entry point?

## Findings

Use a per-user LaunchAgent to keep an event listener running in the logged-in GUI session. The listener invokes `reload-theme` on appearance changes. This still uses a resident process, but it waits for notifications rather than polling.

A small Swift implementation can subscribe to `AppleInterfaceThemeChangedNotification` through `DistributedNotificationCenter`. [bouk/dark-mode-notify's implementation](https://github.com/bouk/dark-mode-notify/blob/main/main.swift) demonstrates this. The notification name is not a documented public appearance API, so treat it as a compatibility risk rather than a guaranteed interface.

An existing alternative is [cormacrelf/dark-notify](https://github.com/cormacrelf/dark-notify). Its README documents installation with `brew install cormacrelf/tap/dark-notify` and invoking a command on changes with `dark-notify -c 'command'`. Its examples show a light/dark argument passed to the command, which any wrapper or argument parser must accommodate.

Apple documents [DistributedNotificationCenter](https://developer.apple.com/documentation/Foundation/DistributedNotificationCenter) separately from [launchd](https://developer.apple.com/documentation/xpc/launchd). This investigation did not establish a documented launchd plist trigger specifically for appearance changes. Use launchd to supervise the listener, not as the notification receiver itself.

## Proposed integration

- Prefer the existing `dark-notify` utility over maintaining a custom Swift listener.
- Run it under a user LaunchAgent with explicit executable paths and the PATH needed by `reload-theme` and its subprocesses.
- Refresh on listener startup as well as changes, and retain the manual tmux binding.
- Keep zsh's in-place refresh handler separate. The event listener triggers `reload-theme`; `reload-theme` updates applications and notifies registered shells.
- Make `reload-theme` tolerate tmux being absent, since the LaunchAgent can run without a tmux server.

Implemented in [Automatic theme updates](../theme-reloading.md). The listener is installed and the `dev.mise.reload-theme` LaunchAgent was applied through mise. Zsh notifications use private FIFOs rather than process signals.
