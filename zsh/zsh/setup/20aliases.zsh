# Aliases
alias ta=_tmux_attach
alias tns="tmux new-session -A -n '' -s"
alias ll='ls -lh'
alias lr='ls -thor'
alias ipy=ipython
alias py=python
alias pydoc='=python -m pydoc'
alias pylab='ipython --pylab'
alias clear-pycs='find ${PWD} -name "*.pyc" -delete'
alias g='git'
alias es='exec $SHELL'
alias sqlite3='rlwrap =sqlite3'
alias ls="ls --color=never --literal --tabsize 0"
alias lsc="ls --color=auto --literal --tabsize 0"

if has_executable rg; then
    alias grep=rg
elif has_executable ag; then
    alias grep=ag
else
    alias grep='grep --color=auto'
fi

if has_executable hub; then
    alias git=hub
    alias g=hub
fi

if has_executable task; then
    alias t=task
fi
