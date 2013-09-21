# vim: ft=zsh
function gi() {
    # This function gets the gitignore results from gitignore.io and prints them
    # to screen
    #
    # Inputs: any extra arguments given on the command line are treated as
    # ignore classes for the website.
    #
    # E.g. > gi linux osx python
    #
    # will translate to http://gitignore.io/api/linux,osx,python
    URL=http://gitignore.io/api/
    ARGS=$(echo $* | sed 's/ /,/g')
    curl ${URL}${ARGS}
}

# From GRB
function mcd() { mkdir -p $1 && cd $1 }
function cdf() { cd *$1*/ }

# Source the custom zshrc.local file in the system
if [[ -f ${HOME}/.zshrc.local ]]; then
    source ${HOME}/.zshrc.local
fi
