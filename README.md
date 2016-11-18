# Nixos configuration

This is my default nixos configuration, commonly used on all my
nixos-enabled computers.

Thanks to @FaustXVI 👼

## How to use

When installing nixos:

- clone this repository in `/etc/nixos`
- create a `configuration.nix` either by linking it from a known
  machine (in `machine/`) or by doing your own.
- run `nixos-generate-configuration` to have the
  `hardware-configuration.nix` generated.
