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

# Enable marking of directories for quick jumping around
# Source: http://jeroenjanssens.com/2013/08/16/quickly-navigate-your-filesystem-from-the-command-line.html
export MARKPATH=$HOME/.marks
function jump { 
    cd -P $MARKPATH/$1 2>/dev/null || echo "No such mark: $1"
}
function mark { 
    mkdir -p $MARKPATH; ln -s $(pwd) $MARKPATH/$1
}
function unmark { 
    rm -i $MARKPATH/$1 
}
function marks {
    \ls -l "$MARKPATH" | tail -n +2 | sed 's/  / /g' | cut -d' ' -f9- | awk -F ' -> ' '{printf "%-10s -> %s\n", $1, $2}'
}

function _completemarks {
    reply=($(ls $MARKPATH))
}
compctl -K _completemarks jump
compctl -K _completemarks unmark

# Add an alias for the jump command
alias j=jump
