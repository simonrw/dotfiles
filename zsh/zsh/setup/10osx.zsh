# OSX setup
case $OSTYPE in
    darwin*)
        export PATH=/Library/TeX/texbin:${GOPATH}/bin:/usr/local/sbin:/sbin:/usr/sbin:${HOME}/.cabal/bin:${PATH}
        alias gvim=mvim
        alias sed=gsed
        alias gview=mview
        alias indent=gindent
        alias lsc="ls -G"

        export work=${HOME}/work
        export NGTS=${work}/NGTS


        # Configure chruby
        # source /usr/local/opt/chruby/share/chruby/chruby.sh

        # Set up online help
        unalias run-help
        autoload run-help
        HELPDIR=/usr/local/share/zsh/helpfiles

        # Set up the keymap for neovim
        tic ~/.zsh/$TERM.ti
        ;;
esac

