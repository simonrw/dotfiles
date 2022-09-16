ARGS ?=
HOSTNAME ?= $(shell hostname -s)

os/switch:
	nix build .#darwinConfigurations.${HOSTNAME}.system
	./result/sw/bin/darwin-rebuild switch --flake '.#${HOSTNAME}' ${ARGS}

switch:
	nix build .#homeConfigurations.simon.activationPackage
	./result/activate

os/build:
	nix build .#darwinConfigurations.${HOSTNAME}.system
	./result/sw/bin/darwin-rebuild build --flake '.#${HOSTNAME}' ${ARGS}
