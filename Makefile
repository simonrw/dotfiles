ARGS ?=
HOSTNAME ?= $(shell hostname -s)

os/switch:
	nix build .#darwinConfigurations.${HOSTNAME}.system
	./result/sw/bin/darwin-rebuild switch --flake '.#${HOSTNAME}' ${ARGS}

switch:
	nix build .#homeConfigurations.${HOSTNAME}.activationPackage
	./result/activate

build:
	nix build .#darwinConfigurations.${HOSTNAME}.system
	./result/sw/bin/darwin-rebuild build --flake '.#${HOSTNAME}' ${ARGS}
