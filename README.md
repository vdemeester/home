# Nixos configuration 🐸

This is my nixos configuration, commonly used on all my
nixos-enabled computers.

## How to use 🐻


## NixOS

When installing nixos:

- clone this repository in `/etc/nixos`
- create a `hostname` with the hostname you want (`echo wakasu > /etc/nixos/hostname`)
- create a `machine/${hostname}.nix` file with the thing you want (look at other ones)
- run `nixos-generate-configuration` to have the
  `hardware-configuration.nix` generated.
  
## On other operating system/distributions

Use [vdemeester/home](https://github.com/vdemeester/home) instead.
