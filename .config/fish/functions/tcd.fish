function tcd --description 'Create a temp directory, cd into it, and start a zmx session'
    cd (mktemp -d $TMPDIR/tempXXXX)
    tnew
end
