# Fish's catppuccin-macchiato theme uses Latte colors in light mode.
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
() {
    local normal command_color comment error escape keyword operator option param quote ending gray selection
    if [[ "$__IS_DARK_THEME" == 1 ]]; then
        normal=cad3f5 command_color=8aadf4 comment=8087a2 error=ed8796
        escape=ee99a0 keyword=c6a0f6 operator=f5bde6 option=a6da95
        param=f0c6c6 quote=a6da95 ending=f5a97f gray=6e738d selection=363a4f
    else
        normal=4c4f69 command_color=1e66f5 comment=8c8fa1 error=d20f39
        escape=e64553 keyword=8839ef operator=ea76cb option=40a02b
        param=dd7878 quote=40a02b ending=fe640b gray=9ca0b0 selection=ccd0da
    fi

    typeset -g ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#$gray"
    typeset -gA ZSH_HIGHLIGHT_STYLES=(
        default "fg=#$param"
        arg0 "fg=#$command_color"
        unknown-token "fg=#$error"
        reserved-word "fg=#$keyword"
        alias "fg=#$command_color"
        suffix-alias "fg=#$command_color"
        global-alias "fg=#$command_color"
        builtin "fg=#$command_color"
        function "fg=#$command_color"
        command "fg=#$command_color"
        hashed-command "fg=#$command_color"
        precommand "fg=#$keyword"
        commandseparator "fg=#$ending"
        path "fg=#$param,underline"
        path_prefix "fg=#$param,underline"
        single-hyphen-option "fg=#$option"
        double-hyphen-option "fg=#$option"
        single-quoted-argument "fg=#$quote"
        double-quoted-argument "fg=#$quote"
        dollar-quoted-argument "fg=#$quote"
        rc-quote "fg=#$escape"
        back-double-quoted-argument "fg=#$escape"
        back-dollar-quoted-argument "fg=#$escape"
        dollar-double-quoted-argument "fg=#$param"
        back-quoted-argument "fg=#$param"
        command-substitution "fg=#$param"
        command-substitution-delimiter "fg=#$operator"
        globbing "fg=#$operator"
        history-expansion "fg=#$operator"
        redirection "fg=#$operator"
        comment "fg=#$comment"
        assign "fg=#$param"
    )
    zle_highlight=("default:fg=#$normal" "region:bg=#$selection" "isearch:bg=#$selection")
}
