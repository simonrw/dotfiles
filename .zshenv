# All zsh invocations, including "ssh host command" and new tmux panes.
# launchd owns the agent; the GUI LaunchAgent publishes this stable symlink.
if [[ "$OSTYPE" == darwin* && -S "$HOME/.ssh/macos-agent.sock" ]]; then
    export SSH_AUTH_SOCK="$HOME/.ssh/macos-agent.sock"
    unset SSH_AGENT_PID
fi
