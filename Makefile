ARGS ?=
HOSTNAME ?= $(shell hostname -s)

switch:
	darwin-rebuild switch --flake '.#${HOSTNAME}' ${ARGS}

build:
	darwin-rebuild build --flake '.#${HOSTNAME}' ${ARGS}
