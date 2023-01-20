# Nix config

I manage my systems with [nix](https://nixos.org):

* [NixOS](https://nixos.org)
* [nix-darwin](https://github.com/LnL7/nix-darwin)
* [home-manager](https://github.com/nix-community/home-manager)

I do not expect anyone else to use this system, but there may be useful information in the configs.

## Usage

*These instructions are for my benefit only*

### Updating the system

```
$ ./bin/switch.sh
```

### Building the system into the result dir

```
$ ./bin/build.sh
```

### Building a VM of the system on NixOS

```
$ ./bin/vm-build.sh
```

### Playing around with the config in a REPL

```
./bin/nixos-config.sh
```

### Updating my home-manager config only

```
./bin/home-switch.sh
```

### Building my home-manager config only

```
./bin/home-build.sh
```
