# Aliases
alias ta=_tmux_attach
alias tns="tmux new-session -A -n '' -s"
alias tsw=tswitch
alias m=make
alias ll='ls -lh'
alias lr='ls -thor'
alias man1='man 1'
alias o='open'
alias irssi="TERM=screen-256color irssi"
alias ipy=ipython
alias ipysh='ipython --profile=sh'
alias py=python
alias pydoc='=python -m pydoc'
alias pylab='ipython --pylab'
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
alias mutt='echo "Use either email-gmail or email-work"'
alias eg=email-gmail  # defined in 30functions.zsh
alias ew=email-work   # defined in 30functions.zsh
alias sqlite3='rlwrap =sqlite3'
alias hn='hn --keep-open'
alias ls="ls --color=never --literal --tabsize 0"
alias lsc="ls --color=auto --literal --tabsize 0"

if has_executable rg; then
    alias grep=rg
    alias ag=rg
elif has_executable ag; then
    alias grep=ag
else
    alias grep='grep --color=auto'
fi

if has_executable hub; then
    alias git=hub
    alias g=hub
fi

if has_executable nvim; then
    alias vim=nvim
fi
