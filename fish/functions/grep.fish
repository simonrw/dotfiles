function grep
	if command -sq rg
		rg $argv
	else if command -sq ag
		ag $argv
	else
		grep $argv
	end
end
