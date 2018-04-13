# Linux setup
case $OSTYPE in
    linux*)
        export LD_LIBRARY_PATH=${BUILD_PREFIX}/lib:${LD_LIBRARY_PATH}
        export PKG_CONFIG_PATH=${BUILD_PREFIX}/lib/pkgconfig:${PKG_CONFIG_PATH}

        # Set up the module command
        function module() { eval `modulecmd zsh $*`; }

        export work=${HOME}/work/
        function open() { nohup xdg-open 2>/dev/null $* & disown }
        alias pbcopy='xsel --clipboard --input'
        alias pbpaste='xsel --clipboard --output'


        alias configurevim='./configure --prefix=$BUILD_PREFIX --with-features=huge --enable-pythoninterp --with-compiledby="Simon Walker" --disable-gui --without-x'
esac
