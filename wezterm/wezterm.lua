local wezterm = require("wezterm")

return {
    color_scheme = "Lucius",
    font = wezterm.font("Source Code Pro Semibold"),
    automatically_reload_config = true,
    freetype_load_target = "HorizontalLcd",
    freetype_load_flags = "DEFAULT",
    enable_tab_bar = false,

    color_schemes = {
        ["Lucius"] = {
            foreground = "#ffffff",
            background = "#181818",

            cursor_bg = "#ffffff",
            cursor_fg = "#000000",
            cursor_border = "#ffffff",
            ansi = {
                '#616261',
                '#fd8272',
                '#b4fa73',
                '#fefcc3',
                '#a5d5fe',
                '#fd8ffd',
                '#d0d1fe',
                '#f1f0f2',
            },
            brights = {
                '#8d8e8d',
                '#fec4bd',
                '#d6fcb9',
                '#fefdd5',
                '#c1e3fe',
                '#fdb1fe',
                '#e5e6fe',
                '#fefffe',
            },
        },
    },
}
