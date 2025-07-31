# Common Hardware Configurations

This directory contains reusable hardware configurations for common system types:

## RPi4 Systems
- `rpi4-boot.nix` - Common boot configuration for Raspberry Pi 4 systems
- `rpi4-simple-fs.nix` - Standard filesystem layout (/ only)
- `rpi4-data-fs.nix` - Filesystem layout with additional /data partition

## Intel Laptops
- `intel-laptop.nix` - Common boot configuration for Intel-based laptops

## Usage

These configurations are imported by individual system hardware.nix files:

```nix
{ ... }:
{
  imports = [
    ../common/hardware/rpi4-boot.nix
  ];
}
```

This reduces code duplication and provides consistent configurations across similar systems.