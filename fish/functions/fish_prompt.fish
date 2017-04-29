function fish_prompt
	set -l njobs (jobs | wc -l | sed 's/ //g')
	set -l status_color

	if test $status -eq 0
		set status_color green
	else
		set status_color red
	end

	set -l prompt_njobs_char
	if test $njobs -eq 0
		set prompt_njobs_char ''
	else
		set prompt_njobs_char '= '
	end

	set -l prompt_symbol (set_color $status_color)"\$"(set_color normal)
	set -l prompt_njobs_symbol (set_color yellow)$prompt_njobs_char(set_color normal)

	printf "\n%s%s " $prompt_njobs_symbol $prompt_symbol
end
