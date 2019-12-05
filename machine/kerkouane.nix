{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ../networking.nix ];
  time.timeZone = "Europe/Paris";
  boot = {
    cleanTmpDir = true;
    loader.grub.enable = true;
  };
  profiles = {
    git.enable = true;
    nix-config.localCaches = [];
    nix-config.buildCores = 1;
    ssh.enable = true;
    syncthing.enable = true;
    wireguard.server.enable = true;
  };
  networking.firewall.allowPing = true;
  services = {
    openssh.ports = [ ssh.kerkouane.port ];
    openssh.permitRootLogin = "without-password";
    syncthing.guiAddress = "${wireguard.ips.kerkouane}:8384";
  };
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGR4dqXwHwPpYgyk6yl9+9LRL3qrBZp3ZWdyKaTiXp0p vincent@shikoku"
  ];
}
