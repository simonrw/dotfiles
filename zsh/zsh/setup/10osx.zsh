# OSX setup
case $OSTYPE in
    darwin*)
        export PATH=/Library/TeX/texbin:${GOPATH}/bin:/usr/local/opt/coreutils/libexec/gnubin:/usr/local/sbin:/sbin:/usr/sbin:${HOME}/.cabal/bin:/usr/local/opt/python/libexec/bin:${PATH}

        alias curl=/usr/local/opt/curl/bin/curl

        # Configure homebrew
        export HOMEBREW_NO_ANALYTICS=1
        # Only update every week
        export HOMEBREW_AUTO_UPDATE_SECS=604800

        export DYLD_LIBRARY_PATH=${BUILD_PREFIX}/lib:${DYLD_LIBRARY_PATH}

        if has_executable gls; then
            alias ls="gls --color=auto"
            alias thor="gls -thor"
        fi

        # Set up the keymap for neovim
        # tic ~/.zsh/$TERM.ti
        ;;
esac

