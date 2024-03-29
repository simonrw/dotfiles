# abbreviations
abbr -a -g ipy ipython
abbr -a -g py python
abbr -a -g g git
abbr -a -g k kubectl
abbr -a -g gp 'git pull'
abbr -a -g gco 'git checkout'
abbr -a -g v vim
abbr -a -g c cargo

# aliases
alias ta=_tmux_attach
alias ll='ls -lh'
alias pylab='ipython --pylab'
alias clear-pycs='find {$PWD} -name "*.pyc" -delete'
alias es='exec $SHELL'
alias sourceenv="source ./venv/bin/activate"
alias vup="nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'"
test -d ~/.ssh && alias add-keys='ssh-add (find ~/.ssh -maxdepth 1 -type f -name "id_rsa*" | grep -v pub | grep -v bak)'
alias gpe="git push && exit"
alias gpr="git pull --rebase"
alias tl="tmux-last"
alias gs="git status"

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

if type -q exa
    alias ls=exa
    alias tree="exa -T"
    alias thor="exa -s modified -l"
    alias lr=thor
else
    alias lr='ls -thor'
end

if type -q task
    abbr -a -g t task
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

if type -q lab
    alias lpb="lab project browse"
    alias mr="lab mr browse"
    alias ci="lab ci view"
end

if type -q paru
    alias pacman=paru
end

if type -q edit-dotfiles
    alias dotfiles=edit-dotfiles
end

if type -q noti
    abbr -a -g n noti
end

if type -q curlie
    alias curl=curlie
    alias http=curlie
end
