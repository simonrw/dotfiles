function is-dark-theme --description 'Return success when the system uses a dark theme'
    if test (uname) = Darwin
        defaults read -g AppleInterfaceStyle >/dev/null 2>&1
        return $status
    end

    if type -q gsettings
        set -l colorscheme (gsettings get org.gnome.desktop.interface color-scheme 2>/dev/null)
        if string match -q '*dark*' -- $colorscheme
            return 0
        else if string match -q '*light*' -- $colorscheme
            return 1
        end

        set -l theme (gsettings get org.gnome.desktop.interface gtk-theme 2>/dev/null)
        string match -q '*dark*' -- $theme; and return 0
    end

    return 1
end
