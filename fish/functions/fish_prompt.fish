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
    set -l status_color (set_color $fish_color_status)
    set -l statusb_color (set_color $bold_flag $fish_color_status)
    set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)
    set -l bgjobs_color (set_color yellow)

    set -l num_bg_jobs (count (jobs))
    set -l jobs_prompt ''
    if test $num_bg_jobs = 0
        set jobs_prompt ''
    else
        set jobs_prompt "= "
    end

    echo -n -s {$bgjobs_color} {$jobs_prompt} {$normal} {$prompt_status} {$suffix} " "
end

function fish_right_prompt --description 'Write out the right prompt'
    date '+%H:%M'
end

# fish shell: Update both normal prompt and right prompt when executing a command
#
# This hook can be used to ensure e.g. a branch name in the main prompt and
# a timestamp in the right prompt reflects when the command was actually
# run, rather than when the prompt was printed.
#
# This version tries to handle multi-line commands, but doesn't print the
# `fish_right_prompt` when doing so.
function reprint_prompt --on-event fish_preexec
    set -l _prompt (fish_prompt)
    set -l _command (fish_indent --no-indent --ansi (echo -n "$argv" | psub))
    set -l _right_prompt (fish_right_prompt | tr -d '\n')
    set -l _len_right_prompt (string length $_right_prompt)
    # Note that both emoji and ANSI escape chars (like colors) in the prompt messes up the length here.
    set -l _len_prompt (string length "$_prompt")
    # `string length` counts the ANSI escape chars, so we take the raw command.
    set -l _len_command (string length "$argv")
    set -l _num_lines (math 'ceil(('$_len_prompt + $_len_command')' / $COLUMNS')')
    # Debug output:
    # echo -e -n "\033[10F                    --> "$_len_prompt $_prompt $_len_command $_command $_num_lines" <--"
    # echo -e -n "\033[10B"
    # Go to previous line (\033[F), clear line and print current time, then go to start of line (\r)
    # Clearing the line is necessary in cases where the prompt has gotten shorter, since it
    # was first printed (e.g. shorter git branch name). In that case, without clearing,
    # parts of the old prompt or command would hang around.
    echo -e -n "\033["$_num_lines"F"(string repeat --no-newline --count (math $COLUMNS - $_len_right_prompt) ' ')$_right_prompt'\r'
    # Print prompt (to get current git branch)
    echo "$_prompt""$_command"
end
