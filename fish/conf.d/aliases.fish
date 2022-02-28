function has_executable --argument-names command
    if not test -n "$command"
        echo "has_executable requires one argument" >&2
        return 1
    end

    command -v "$command" >/dev/null 2>/dev/null
end

alias ta=_tmux_attach
alias ll='ls -lh'
alias e="$EDITOR"
alias lr='ls -thor'
alias ipy=ipython
alias py=python
alias pydoc='=python -m pydoc'
alias pylab='ipython --pylab'
alias clear-pycs='find {$PWD} -name "*.pyc" -delete'
alias g='git'
alias gs="git status"
alias ga='git commit --amend'
alias es='exec $SHELL'
alias k=kubectl
alias sourceenv="source ./venv/bin/activate"
alias vup="nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'"
test -d ~/.ssh && alias add-keys='ssh-add (find ~/.ssh -maxdepth 1 -type f -name "id_rsa*" | grep -v pub | grep -v bak)'
alias gpe="git push && exit"
alias gpr="git pull --rebase"
alias pip='python3 -m pip'
alias tl="tmux-last"

if has_executable git-change
    alias gc="git change"
end
 
if has_executable rg
    alias grep=rg
else if has_executable ag
    alias grep=ag
else
    alias grep='grep --color=auto'
end

if has_executable task
    alias t=task
end

if has_executable bat
    alias cat=bat
    alias less=bat
    alias more=bat
end

if has_executable nvim
    alias vim=nvim
    alias vi=nvim
    alias vimdiff="nvim -d"
end

if has_executable glab
    alias lpb="glab project view -w"
    alias mr="glab mr view -w"
    alias ci="glab ci view"
end

if has_executable paru
    alias pacman=paru
end

if has_executable edit-dotfiles
    alias dotfiles=edit-dotfiles
end

if has_executable xh
    alias http=xh
end

if has_executable gojq
    alias jq=gojq
end

if has_executable noti
    alias n=noti
end
