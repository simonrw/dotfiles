ARGS ?=

switch:
	home-manager switch --flake '.#simon' ${ARGS}

build:
	home-manager build --flake '.#simon' ${ARGS}
