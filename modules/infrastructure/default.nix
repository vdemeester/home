# Infrastructure modules for managing machine metadata, services, and network configuration
{
  imports = [
    ./machine.nix
    ./machines-registry.nix
    ./syncthing-folders.nix
    ./services.nix
    ./dns.nix
    ./vpn.nix
    ./users.nix
  ];
}
