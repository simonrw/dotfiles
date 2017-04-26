function fs
	if test -z $TMUX
		set CMD "tmux attach -t \$(tmux list-sessions -F '#{session_name}' | fzf-tmux)"
	else
		set CMD "tmux switch-client -t \$(tmux list-sessions -F '#{session_name}' | fzf-tmux)"
	end

	# Have to resort to bash for this :(
	bash -c "$CMD"
end
