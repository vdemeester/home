{ pkgs, prefix, ... }:

{
  imports = [ ./base.nix ];
  profiles.containers = {
    enable = true;
    docker = false;
    podman = true;
  };
}
