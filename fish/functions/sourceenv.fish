function sourceenv
	if test -f venv/bin/conda
		echo "Sourceenv does not work with conda environments, sorry :("
	else
		source $PWD/venv/bin/activate.fish
	end
end
