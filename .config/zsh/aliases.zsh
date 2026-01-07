alias add-keys='ssh-add $(find ~/.ssh - maxdepth 1 - type f - name "id_rsa*" | grep - v pub | grep - v bak)'
alias c=cargo
alias clear-pycs='find { $PWD } -name '\''*.pyc'\'' -delete'
alias es='exec $SHELL'
alias eza='eza --group-directories-first --header'
alias fs=tmux-session-history
alias g=git
alias gcpr='gh pr create -a @me --label '\''semver: patch'\'''
alias gcprd='gh pr create -a @me --draft --label '\''semver: patch'\'''
alias gpr='git pull --rebase'
alias grep=rg
alias gs=gss  # This command shadows ghostscript but if I alias it it only affects my interactive shell 
alias k=kubectl
alias la='eza -a'
alias ll='eza -l'
alias lla='eza -la'
alias lr=thor
alias ls=eza
alias lt='eza --tree'
alias notes='open -a Emacs ~/notes.org'
alias ntfy=notify-wrapper
alias nvim=nvim
alias project=listprojects
alias ptl='pytest $(testsearch rerun -l)'
alias pts='pytest $(testsearch)'
alias pydoc='python -m pydoc'
alias pylab='ipython - -pylab'
alias sourceenv='source ./venv/bin/activate'
alias thor='eza -s modified -l'
alias tl=tmux-last
alias tree='eza -T'
alias vim=nvim
alias vimdiff='nvim -d'
alias vup='nvim --headless "+Lazy! sync" +qa'
alias watch=viddy

# handle os specific binaries
case $OSTYPE in
    darwin*)
        alias cat=bat
        alias less=bat
        alias more=bat
        ;;
    *)
        ;;
esac
