{ config, pkgs, ... }:

{
  imports = [
    ../networking.nix # generated at runtime by nixos-infect
  ];
  time.timeZone = "Europe/Paris";
  boot = {
    cleanTmpDir = true;
    loader.grub.enable = true;
  };
  profiles = {
    git.enable = true;
    ssh.enable = true;
    syncthing.enable = true;
  };
  programs = {
    podman = true;
  };
  networking.firewall.allowPing = true;
  services = {
    openssh.ports = with import ../assets/machines.nix; [ ssh.carthage.port ];
    openssh.permitRootLogin = "without-password";
    syncthing-edge.guiAddress = with import ../assets/machines.nix; "${wireguard.ips.carthage}:8384";
    wireguard = with import ../assets/machines.nix; {
      enable = true;
      ips = [ "${wireguard.ips.carthage}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };  
  };
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGR4dqXwHwPpYgyk6yl9+9LRL3qrBZp3ZWdyKaTiXp0p vincent@shikoku"
  ];
}
