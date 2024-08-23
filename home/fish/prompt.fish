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
set -l status_color (set_color $fish_color_status)
set -l statusb_color (set_color $bold_flag $fish_color_status)
set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)

set -l num_bg_jobs (count (jobs))
set -l suffix_color \'\'
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

echo -n -s {$nix_shell_str} {$suffix_color} {$suffix} {$normal} " "
