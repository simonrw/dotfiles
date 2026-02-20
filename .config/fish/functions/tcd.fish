function tcd --description 'Create a temp directory, cd into it, and start a tmux session'
    cd (mktemp -d $TMPDIR/tempXXXX)
    tnew
end
