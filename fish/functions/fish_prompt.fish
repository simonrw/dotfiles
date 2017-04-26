function fish_prompt
	set -l status_color
	if test $status -eq 0
		set status_color green
	else
		set status_color red
	end

	set -l prompt_symbol (set_color $status_color)"\$"(set_color normal)

	printf "\n%s " $prompt_symbol
end
