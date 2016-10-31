# Aliases
alias tmux='tmux -2'
alias ta=_tmux_attach
alias tns="tmux new-session -As"
alias tsw=tswitch
alias m=make
alias ls="${__LS_COMMAND} --color=never --literal --tabsize 0"
alias lsc="${__LS_COMMAND} --color=auto --literal --tabsize 0"
alias ll='ls -lh'
alias lr='ls -thor'
alias man1='man 1'
alias o='open'
alias irssi="TERM=screen-256color irssi"
alias ipy=ipython
alias ipysh='ipython --profile=sh'
alias py=python
alias pydoc='=python -m pydoc'
alias pylab='ipython --profile=pylab'
alias clear-pycs='find ${PWD} -name "*.pyc" -delete'
alias g='git'
alias es='exec $SHELL'
alias v=vim
alias work="cd ${work} && pwd && ls"
alias ngts="cd ${NGTS} && pwd && ls"
alias wasp="cd ${WASP} && pwd && ls"
alias dotfiles="cd ${HOME}/dotfiles && pwd"
alias dev="cd ${HOME}/dev && pwd && ls"
alias de='direnv edit .'
alias da='direnv allow'
alias dr='direnv reload'
alias grep='grep --color=auto'
alias mutt='echo "Use either mutt-gmail or mutt-work"'
alias email-gmail='=mutt -n -F ~/.mutt.d/startup-gmail.mutt'
alias email-work='=mutt -n -F ~/.mutt.d/startup-work.mutt'
alias email-ngtsops='=mutt -n -F ~/.mutt.d/startup-ngtsops.mutt'
alias eg=email-gmail
alias ew=email-work
alias en=email-ngtsops
alias sqlite3='rlwrap =sqlite3'
alias hn='hn --keep-open'
alias f=fg

# If hub exists, run this instead of git
if has_executable hub; then
    alias git=hub
fi

# NGTS aliases
alias par-ds='ssh par-ds'
alias ngts_ops='ngtsdb ngts_ops'
alias ngts_pipe='ngtsdb ngts_pipe'
alias irc='ssh -t raspberrypi tmux attach -t irc'

if has_executable rg; then
    alias ag=rg
fi
