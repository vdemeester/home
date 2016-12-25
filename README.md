# Nixos configuration

This is my default nixos configuration, commonly used on all my
nixos-enabled computers.

Thanks to @FaustXVI 👼

## How to use

When installing nixos:

- clone this repository in `/etc/nixos`
- create a `hostname` with the hostname you want
- create a `machine/${hostname}.nix` file with the thing you want (look at other ones)
- run `nixos-generate-configuration` to have the
  `hardware-configuration.nix` generated.
