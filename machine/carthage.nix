{ config, pkgs, ... }:

with import ../assets/machines.nix; {
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
    nix-config = {
      autoUpgrade = false;
      localCaches = [];
    };
    ssh.enable = true;
    syncthing.enable = true;
  };
  networking.firewall.allowPing = true;
  services = {
    openssh.ports = [ ssh.carthage.port ];
    openssh.permitRootLogin = "without-password";
    syncthing-edge.guiAddress = "${wireguard.ips.carthage}:8384";
    wireguard = {
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
