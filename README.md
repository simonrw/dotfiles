# Nix config

I manage my systems with [nix](https://nixos.org):

* [NixOS](https://nixos.org)
* [nix-darwin](https://github.com/LnL7/nix-darwin)
* [home-manager](https://github.com/nix-community/home-manager)

I do not expect anyone else to use this system, but there may be useful information in the configs.

## Layout

* nixos system configurations under [`system/nixos`](./system/nixos)
  * my main system ["astoria"](./system/nixos/astoria) is the configuration of my main development PC (intel CPU, nVidia GPU)
* nix-darwin configurations under [`system/darwin`](./system/darwin)
* home-manager configuration under [`home`](./home) with "top level" configuration at [`home/home.nix`](./home/home.nix) with imports to other sub-modules. I am trying to migrate to a more modular configuration as part of learning nix, but I'm not 100% there yet

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
