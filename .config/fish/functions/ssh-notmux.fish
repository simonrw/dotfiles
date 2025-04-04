function ssh-notmux --description="SSH into a host disabling automatic tmux on the destination host" --wraps="ssh"
    if test (count $argv) -gt 1
        ssh -t $argv[1] env TMUX_DISABLED=1 $argv[2..-1]
    else
        ssh -t $argv[1] env TMUX_DISABLED=1 fish
    end
end
