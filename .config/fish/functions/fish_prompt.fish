function fish_prompt
    set -l last_status $status
    echo
    if set -q ZMX_SESSION
        set_color blue
        printf '[%s] ' "$ZMX_SESSION"
        set_color normal
    end
    if test $last_status -eq 0
        set_color green
    else
        set_color red
    end
    printf '$ '
    set_color normal
end
