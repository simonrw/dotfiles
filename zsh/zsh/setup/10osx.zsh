# OSX setup
case $OSTYPE in
    darwin*)
        export PATH=/opt/homebrew/bin:/usr/local/opt/coreutils/libexec/gnubin:/sbin:/usr/sbin:/usr/local/opt/curl/bin:${PATH}

        # Configure homebrew
        export HOMEBREW_NO_ANALYTICS=1
        export HOMEBREW_NO_INSTALL_CLEANUP=1
        export HOMEBREW_NO_AUTO_UPDATE=1
        export HOMEBREW_NO_INSTALL_UPGRADE=1

        eval $(brew shellenv)

        export DYLD_LIBRARY_PATH=${BUILD_PREFIX}/lib:${DYLD_LIBRARY_PATH}

        if has_executable gls; then
            alias ls="gls --color=auto"
            alias thor="gls -thor"
        fi

        # Set up the keymap for neovim
        # tic ~/.zsh/$TERM.ti
        ;;
esac

