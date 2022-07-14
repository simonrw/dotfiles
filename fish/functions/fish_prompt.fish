function fish_prompt --description 'Write out the prompt'
    set -l last_pipestatus $pipestatus
    set -lx __fish_last_status $status # Export for __fish_print_pipestatus.
    set -l normal (set_color normal)
    set -q fish_color_status
    or set -g fish_color_status --background=red white

    # Color the prompt differently when we're root
    set -l color_cwd $fish_color_cwd
    set -l suffix '$'
    if functions -q fish_is_root_user; and fish_is_root_user
        if set -q fish_color_cwd_root
            set color_cwd $fish_color_cwd_root
        end
        set suffix '#'
    end

    # Write pipestatus
    # If the status was carried over (e.g. after `set`), don't bold it.
    set -l bold_flag --bold
    set -q __fish_prompt_status_generation; or set -g __fish_prompt_status_generation $status_generation
    if test $__fish_prompt_status_generation = $status_generation
        set bold_flag
    end
    set __fish_prompt_status_generation $status_generation

    set -l num_bg_jobs (count (jobs))
    set -l suffix_color ''
    if test $__fish_last_status = 0
        if test $num_bg_jobs = 0
            set suffix_color (set_color green)
        else
            set suffix_color (set_color -u green)
        end
    else
        if test $num_bg_jobs = 0
            set suffix_color (set_color red)
        else
            set suffix_color (set_color -u red)
        end
    end

    echo
    echo -s (set_color brblue) (prompt_pwd --dir-length 2 --full-length-dirs 1) (set_color reset)
    echo -n -s (__present_time) ' ' {$bgjobs_color} {$jobs_prompt} {$suffix_color} {$suffix} {$normal} " "
end

function __present_time
    echo -n -s (set_color black) '[' (date +%H:%M) ']' (set_color reset)
end
