# Configure SSH agent to start on boot
if [ -f ~/.ssh/agent.env ]; then
    . ~/.ssh/agent.env >/dev/null
    if ! kill -0 $SSH_AGENT_PID >/dev/null 2>&1; then
        echo "Stale agent file found. Spawning new agent..." >&2
        eval $(ssh-agent | tee ~/.ssh/agent.env)
    fi
else
    echo "Starting ssh agent"
    eval $(ssh-agent | tee ~/.ssh/agent.env)
fi
