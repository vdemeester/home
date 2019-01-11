{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ./home.nix ];
  boot = {
    cleanTmpDir = true;
  };
  networking.firewall.allowPing = true;
  nix = {
    distributedBuilds = true;
    buildMachines = [{
      hostName = "hokkaido.home";
      sshUser = "vincent";
      sshKey = "/home/vincent/.ssh/id_ed25519";
      system = "x86_64-linux";
      maxJobs = 2;
      sypportedFeatures = ["kvm" "docker"];
    } {
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
    coredns = {
      enable = true;
      names = dns;
    };
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
