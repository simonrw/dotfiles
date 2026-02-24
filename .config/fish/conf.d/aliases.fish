# Simple abbreviations (expand inline)
abbr --add c cargo
abbr --add es 'exec fish'
abbr --add fs tmux-session-history
abbr --add g git
abbr --add gcpr "gh pr create -a @me --label 'semver: patch'"
abbr --add gcprd "gh pr create -a @me --draft --label 'semver: patch'"
abbr --add gpr 'git pull --rebase'
abbr --add gs gss
abbr --add k kubectl
abbr --add ntfy notify-wrapper
abbr --add project listprojects
abbr --add pydoc 'python -m pydoc'
abbr --add pylab 'ipython --pylab'
abbr --add sourceenv 'source ./venv/bin/activate.fish'
abbr --add tl tmux-last
abbr --add vim nvim
abbr --add vimdiff 'nvim -d'
abbr --add vup 'nvim --headless "+Lazy! sync" +qa'
abbr --add watch viddy

# eza wrappers (alias so they replace silently)
alias ls 'eza --group-directories-first --header'
alias la 'eza --group-directories-first --header -a'
alias ll 'eza --group-directories-first --header -l'
alias lla 'eza --group-directories-first --header -la'
alias lt 'eza --group-directories-first --header --tree'
alias lr 'eza --group-directories-first --header -s modified -l'
alias thor 'eza --group-directories-first --header -s modified -l'
alias tree 'eza --group-directories-first --header -T'
alias grep rg
alias notes 'open -a Emacs ~/notes.org'

alias cat bat
alias less bat
alias more bat

# Functions for aliases with command substitution
function add-keys
    ssh-add (find ~/.ssh -maxdepth 1 -type f -name "id_rsa*" | command grep -v pub | command grep -v bak)
end

function clear-pycs
    find $PWD -name '*.pyc' -delete
end

function ptl
    pytest (testsearch rerun -l)
end

function pts
    pytest (testsearch)
end
