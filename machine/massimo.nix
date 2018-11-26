{ config, pkgs, ... }:

{
  time.timeZone = "Europe/Paris";
  boot = {
    cleanTmpDir = true;
  };
  profiles = {
    git.enable = true;
    ssh.enable = true;
    syncthing.enable = true;
  };
  networking.firewall.allowPing = true;
  services = {
    logind.extraConfig = "HandleLidSwitch=ignore";
    syncthing-edge.guiAddress = with import ../assets/machines.nix; "${wireguard.ips.massimo}:8384";
    wireguard = with import ../assets/machines.nix; {
      enable = true;
      ips = [ "${wireguard.ips.massimo}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
  };
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGR4dqXwHwPpYgyk6yl9+9LRL3qrBZp3ZWdyKaTiXp0p vincent@shikoku"
  ];
}
