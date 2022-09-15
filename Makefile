ARGS ?=
HOSTNAME ?= $(shell hostname -s)

switch:
	nix build .#darwinConfigurations.${HOSTNAME}.system
	./result/sw/bin/darwin-rebuild switch --flake '.#${HOSTNAME}' ${ARGS}

build:
	nix build .#darwinConfigurations.${HOSTNAME}.system
	./result/sw/bin/darwin-rebuild build --flake '.#${HOSTNAME}' ${ARGS}
