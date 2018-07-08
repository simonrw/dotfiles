# OSX setup
case $OSTYPE in
    darwin*)
        export PATH=/Library/TeX/texbin:${GOPATH}/bin:/usr/local/sbin:/sbin:/usr/sbin:${HOME}/.cabal/bin:${PATH}
        alias sed=gsed
        alias indent=gindent

        # Configure homebrew
        export HOMEBREW_NO_ANALYTICS=1
        # Only update every week
        export HOMEBREW_AUTO_UPDATE_SECS=604800

        # Set up the keymap for neovim
        tic ~/.zsh/$TERM.ti
        ;;
esac

