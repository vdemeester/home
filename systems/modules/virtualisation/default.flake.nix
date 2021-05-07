# Virtualisation is grouping modules related to virtualisation, such
# as containers (podman, docker, …), vm (qemu, libvirt, …).
{ lib, ... }:

{
  imports = [
    ./libvirt.nix
  ];
}
