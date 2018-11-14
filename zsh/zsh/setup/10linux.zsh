# Linux setup
case $OSTYPE in
    linux*)
        alias open=xdg-open
        alias pbcopy='xsel --clipboard --input'
        alias pbpaste='xsel --clipboard --output'
esac
