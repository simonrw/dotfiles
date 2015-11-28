# OSX setup
case $OSTYPE in
    darwin*)
        export PATH=/Library/TeX/texbin:${GOPATH}/bin:/usr/local/sbin:/sbin:/usr/sbin:${HOME}/.cabal/bin:${PATH}
        alias gvim=mvim
        alias gview=mview

        # Add GNU coreutils to PATH
        PATH="/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/gnu-tar/libexec/gnubin:/usr/local/opt/gnu-sed/libexec/gnubin:${PATH}"
        MANPATH="/usr/local/opt/coreutils/libexec/gnuman:${MANPATH}"

        export work=${HOME}/work
        export NGTS=${work}/NGTS


        # Configure chruby
        source /usr/local/opt/chruby/share/chruby/chruby.sh

        # Set up online help
        unalias run-help
        autoload run-help
        HELPDIR=/usr/local/share/zsh/helpfiles
        ;;
esac

