# All zsh invocations, including "ssh host command" and new tmux panes.
# launchd owns the agent; the GUI LaunchAgent publishes this stable symlink.
if [[ "$OSTYPE" == darwin* && -S "$HOME/.ssh/macos-agent.sock" ]]; then
    export SSH_AUTH_SOCK="$HOME/.ssh/macos-agent.sock"
    unset SSH_AGENT_PID
fi

export T3CODE_TELEMETRY_ENABLED=false

# Non-interactive shells (agents, scripts) skip `mise activate`, so cargo would bypass mbx.
if [[ ! -o interactive ]]; then
    path=("${MISE_DATA_DIR:-$HOME/.local/share/mise}"/{command-wrappers/bin,shims} $path)
fi
