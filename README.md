# Nixos configuration

This is my default nixos configuration, commonly used on all my
nixos-enabled computers.

Thanks to @FaustXVI 👼

## How to use

When installing nixos:

- clone this repository in `/etc/nixos`
- run `nixos-generate-configuration` to have the
  `hardware-configuration.nix` generated.
- create a `local-configuration.nix` with completely local
  configuration. I mainly use this for `networking.hostname` key.
