function grep
	if command -s rg
		rg $argv
	else if command -s ag
		ag $argv
	else
		grep $argv
	end
end
