if status is-interactive
    set -U fish_greeting

    # set prompt
    set -U fish_color_normal normal
    set -U fish_color_redirection 00afff
    set -U fish_color_end 009900
    set -U fish_color_command 87afff
    set -U fish_color_error ff0000
    set -U fish_color_quote afd75f
    set -U fish_color_param 00afff
    set -U fish_color_match --background=brblue
    set -U fish_color_comment d70000
    set -U fish_color_selection white --bold --background=brblack
    set -U fish_color_search_match bryellow --background=brblack
    set -U fish_color_history_current --bold
    set -U fish_color_operator 00a6b2
    set -U fish_color_escape 00a6b2
    set -U fish_color_cwd green
    set -U fish_color_cwd_root red
    set -U fish_color_valid_path --underline
    set -U fish_color_autosuggestion 9e9e9e
    set -U fish_color_user brgreen
    set -U fish_color_host normal
    set -U fish_color_cancel -r
    set -U fish_pager_color_completion normal
    set -U fish_pager_color_description B3A06D yellow
    set -U fish_pager_color_prefix normal --bold --underline
    set -U fish_pager_color_progress brwhite --background=cyan
end

# this needs to be set for tmux to pick up
set -x SHELL (command -v fish)
