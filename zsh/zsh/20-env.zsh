# vim: ft=zsh
export PATH=${HOME}/.bin:${HOME}/prefix/bin:$PATH
export EDITOR=vim_nox
export VISUAL=${EDITOR}
export BIBINPUTS=${HOME}/work/central-bibliography:${BIBINPUTS}
export TERM=screen-256color

# Taken from grb's zshrc

# By default, zsh considers many characters part of a word (e.g., _ and -).
# Narrow that down to allow easier skipping through words via M-f and M-b.
export WORDCHARS='*?[]~&;!$%^<>'
