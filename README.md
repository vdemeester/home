# Nixos configuration 🐸

This is my nixos configuration, commonly used on all my
nixos-enabled computers **or** nixpkgs-enabled computers.

It is heavily customized and uses
[home-manager](https://github.com/rycee/home-manager/) to manager my
default user packages and configurations files (a.k.a dotfiles).

## How to use 🐻


## NixOS

When installing nixos:

- clone this repository in `/etc/nixos`
- create a `hostname` with the hostname you want (`echo wakasu > /etc/nixos/hostname`)
- create a `machine/${hostname}.nix` file with the thing you want (look at other ones)
- run `nixos-generate-configuration` to have the
  `hardware-configuration.nix` generated.
  
## On other operating system/distributions

When installing it on a non-nixos operating system, the requirements
are to have nix installed.

- `curl https://nixos.org/nix/install | sh` to install it
- clone the configuration in `$HOME/.config/home-manager`
- install my customized *home-manager-helper* : 
  `nix-env -i -f https://github.com/vdemeester/home-manager-helper/archive/master.tar.gz`
- run `hm` to bootstrap it
- run `hm build remote-env` to build a dry-run of the `remote-env`,
  and `hm switch remote-env` to activate it.

The `envs` folder contains different environment either named using
machine (`wakasu`, `hokkaido`, …) or more generic environment.
