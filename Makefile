ARGS ?=
HOSTNAME ?= $(shell hostname -s)

.PHONY: switch
switch: os/switch home/switch

.PHONY: os/switch
os/switch:
	nix build .#darwinConfigurations.${HOSTNAME}.system
	./result/sw/bin/darwin-rebuild switch --flake '.#${HOSTNAME}' ${ARGS}

.PHONY: home/switch
home/switch:
	nix build .#homeConfigurations.simon.activationPackage
	./result/activate

.PHONY: os/build
os/build:
	nix build .#darwinConfigurations.${HOSTNAME}.system
	./result/sw/bin/darwin-rebuild build --flake '.#${HOSTNAME}' ${ARGS}
