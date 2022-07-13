switch (uname)
    case Darwin
        set -x PATH /opt/homebrew/bin /usr/local/bin {$PATH}

        # configure homebrew
        set -x HOMEBREW_NO_ANALYTICS 1
        set -x HOMEBREW_NO_INSTALL_CLEANUP 1
        set -x HOMEBREW_NO_AUTO_UPDATE 1
        set -x HOMEBREW_NO_INSTALL_UPGRADE 1

        eval (brew shellenv)

        set -x DYLD_LIBRARY_PATH {$BUILD_PREFIX}/lib {$DYLD_LIBRARY_PATH}

        if not type -q exa
            if type -q gls
                alias ls "gls --color=auto"
                alias thor "gls -thor"
            end
        end
    case '*'
end
