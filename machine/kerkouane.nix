{ config, pkgs, ... }:

{
  imports = [
    ../networking.nix # generated at runtime by nixos-infect
    ../profiles/wireguard.server.nix
  ];

  profiles.ssh.enable = true;
  profiles.git.enable = true;

  boot.cleanTmpDir = true;
  boot.loader.grub.enable = true;
  networking.firewall.allowPing = true;
  services.openssh.enable = true;
  services.openssh.ports = with import ../assets/machines.nix; [ ssh.kerkouane.port ];
  services.openssh.permitRootLogin = "without-password";
  programs.fish.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGR4dqXwHwPpYgyk6yl9+9LRL3qrBZp3ZWdyKaTiXp0p vincent@shikoku"
  ];
  time.timeZone = "Europe/Paris";
}
