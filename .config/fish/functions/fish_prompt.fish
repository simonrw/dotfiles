function fish_prompt
    set -l last_status $status
    echo
    if test $last_status -eq 0
        set_color green
    else
        set_color red
    end
    printf '$ '
    set_color normal
end
