# Silent replacements and shell functions live here.
# Fish-style inline expansions live in ~/.config/zsh/abbreviations.

alias la='eza --group-directories-first --header -a'
alias ll='eza --group-directories-first --header -l'
alias lla='eza --group-directories-first --header -la'
alias lr='eza --group-directories-first --header -s modified -l'
alias ls='eza --group-directories-first --header'
alias lt='eza --group-directories-first --header --tree'
alias notes='open -a Emacs ~/notes.org'
alias thor='eza --group-directories-first --header -s modified -l'
alias tree='eza --group-directories-first --header -T'
alias pi-work="PI_CODING_AGENT_DIR=$HOME/work/localstack/.pi-localstack pi"

add-keys() {
    ssh-add "${(@f)$(find ~/.ssh -maxdepth 1 -type f -name "id_rsa*" | command rg -v 'pub|bak')}"
}

clear-pycs() {
    find "$PWD" -name '*.pyc' -or -name '__pycache__' -delete
}

ptl() {
    pytest "${(@f)$(testsearch rerun -l)}"
}

pts() {
    pytest "${(@f)$(testsearch)}"
}

octo() {
    nvim -c "Octo $*"
}
