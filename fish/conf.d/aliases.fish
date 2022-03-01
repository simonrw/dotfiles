alias ta=_tmux_attach
alias ll='ls -lh'
alias lr='ls -thor'
abbr -a ipy ipython
abbr -a py python
alias pydoc='=python -m pydoc'
alias pylab='ipython --pylab'
alias clear-pycs='find {$PWD} -name "*.pyc" -delete'
abbr -a g git
alias es='exec $SHELL'
abbr -a k kubectl
alias sourceenv="source ./venv/bin/activate"
alias vup="nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'"
test -d ~/.ssh && alias add-keys='ssh-add (find ~/.ssh -maxdepth 1 -type f -name "id_rsa*" | grep -v pub | grep -v bak)'
alias gpe="git push && exit"
alias gpr="git pull --rebase"
alias pip='python3 -m pip'
alias tl="tmux-last"

if type -q git-change
    alias gc="git change"
end
 
if type -q rg
    alias grep=rg
else if type -q ag
    alias grep=ag
else
    alias grep='grep --color=auto'
end

if type -q task
    abbr -a t task
end

if type -q bat
    alias cat=bat
    alias less=bat
    alias more=bat
end

if type -q nvim
    alias vim=nvim
    alias vi=nvim
    alias vimdiff="nvim -d"
end

if type -q glab
    alias lpb="glab project view -w"
    alias mr="glab mr view -w"
    alias ci="glab ci view"
end

if type -q paru
    alias pacman=paru
end

if type -q edit-dotfiles
    alias dotfiles=edit-dotfiles
end

if type -q xh
    alias http=xh
end

if type -q gojq
    alias jq=gojq
end

if type -q noti
    alias n=noti
end
