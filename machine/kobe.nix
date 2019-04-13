{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ./home.nix ];
  boot = {
    cleanTmpDir = true;
  };
  networking.firewall = {
    allowPing = true;
    allowedUDPPorts = [ 53 ];
    allowedTCPPorts = [ 53 ];
  };
  nix = {
    distributedBuilds = true;
    buildMachines = [{
      hostName = "honshu.home";
      sshUser = "vincent";
      sshKey = "/home/vincent/.ssh/id_ed25519";
      system = "x86_64-linux";
      maxJobs = 2;
    }];
  };
  profiles = {
    avahi.enable = true;
    git.enable = true;
    nix-config.buildCores = 1;
    ssh.enable = true;
  };
  services = {
    bind = {
      enable = true;
      forwarders = [ "8.8.8.8" "8.8.4.4" ];
      cacheNetworks = [ "192.168.12.0/24" "127.0.0.0/8" "10.100.0.0/24" ];
      zones = [
        {
          # home
          name = "home";
          slaves = [];
          file = ../assets/db.home;
        }
        {
          # home.reverse
          name = "192.168.12.in-addr.arpa";
          slaves = [];
          file = ../assets/db.192.168.12;
        }
        {
          # vpn
          name = "vpn";
          slaves = [];
          file = ../assets/db.vpn;
        }
        {
          # vpn.reverse
          name = "10.100.0.in-addr.arpa";
          slaves = [];
          file = ../assets/db.10.100.0;
        }
      ];
    };
    /*
    coredns = {
      enable = true;
      names = dns;
    };
    */
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.kobe}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
  };
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGR4dqXwHwPpYgyk6yl9+9LRL3qrBZp3ZWdyKaTiXp0p vincent@shikoku"
  ];
}
