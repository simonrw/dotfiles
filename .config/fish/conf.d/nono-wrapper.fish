if type -q nono
    function nono
        set -l theme latte
        if test "$__IS_DARK_THEME" = 1
            set theme mocha
        end
        NONO_THEME=$theme command nono $argv
    end
end
