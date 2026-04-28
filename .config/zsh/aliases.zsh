# Silent replacements and shell functions live here.
# Fish-style inline expansions live in ~/.config/zsh/abbreviations via zsh-abbr.

if command -v csm-codex >/dev/null 2>&1; then
    alias codex=csm-codex
fi

alias eza='eza --group-directories-first --header'
alias la='eza --group-directories-first --header -a'
alias ll='eza --group-directories-first --header -l'
alias lla='eza --group-directories-first --header -la'
alias lr='eza --group-directories-first --header -s modified -l'
alias ls='eza --group-directories-first --header'
alias lt='eza --group-directories-first --header --tree'
alias notes='open -a Emacs ~/notes.org'
alias thor='eza --group-directories-first --header -s modified -l'
alias tree='eza --group-directories-first --header -T'

add-keys() {
    ssh-add $(find ~/.ssh -maxdepth 1 -type f -name "id_rsa*" | command grep -v pub | command grep -v bak)
}

clear-pycs() {
    find "$PWD" -name '*.pyc' -delete
}

ptl() {
    pytest $(testsearch rerun -l)
}

pts() {
    pytest $(testsearch)
}

octo() {
    nvim -c "Octo $*"
}
