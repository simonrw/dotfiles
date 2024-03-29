# Aliases
alias ta=_tmux_attach
alias ll='ls -lh'
alias e="$EDITOR"
alias lr='ls -thor'
alias ipy=ipython
alias py=python
alias pydoc='=python -m pydoc'
alias pylab='ipython --pylab'
alias clear-pycs='find ${PWD} -name "*.pyc" -delete'
alias g='git'
alias gs="git status"
alias ga='git commit --amend'
alias es='exec $SHELL'
alias k=kubectl
alias sourceenv="source ./venv/bin/activate"
alias vup="nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'"
test -d ~/.ssh && {
    alias add-keys="ssh-add $(find ~/.ssh -maxdepth 1 -type f -name "id_rsa*" | grep -v pub | grep -v bak)"
}
alias gpe="git push && exit"
alias gpr="git pull --rebase"
alias pip='python3 -m pip'
alias tl="tmux-last"

if has_executable git-change; then
    alias gc="git change"
fi

if has_executable rg; then
    alias grep=rg
elif has_executable ag; then
    alias grep=ag
else
    alias grep='grep --color=auto'
fi

if has_executable task; then
    alias t=task
fi

if has_executable bat; then
    alias cat=bat
    alias less=bat
    alias more=bat
fi

if has_executable nvim; then
    alias vim=nvim
    alias vi=nvim
    alias vimdiff="nvim -d"
fi

if has_executable glab; then
    alias lpb="glab project view -w"
    alias mr="glab mr view -w"
    alias ci="glab ci view"
fi

if has_executable paru; then
    alias pacman=paru
fi

if has_executable edit-dotfiles; then
    alias dotfiles=edit-dotfiles
fi

if has_executable xh; then
    alias http=xh
fi

if has_executable gojq; then
    alias jq=gojq
fi

if has_executable noti; then
    alias n=noti
fi
