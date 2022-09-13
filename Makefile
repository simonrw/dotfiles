ARGS ?=

switch:
	darwin-rebuild switch --flake '.#mba' ${ARGS}

build:
	darwin-rebuild build --flake '.#mba' ${ARGS}
