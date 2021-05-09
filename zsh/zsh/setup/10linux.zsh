# Linux setup
case $OSTYPE in
    linux*)
        alias pbcopy='xsel --clipboard --input'
        alias pbpaste='xsel --clipboard --output'
        alias ls="ls --color=auto"
esac
