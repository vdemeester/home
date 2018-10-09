{ config, pkgs, ... }: {
  imports = [
    ../hardware-configuration.nix
    ../networking.nix # generated at runtime by nixos-infect
    ../profiles/server.nix
    ../profiles/gitconfig.nix
    ../profiles/users.nix
    ../profiles/wireguard.server.nix
  ];

  boot.cleanTmpDir = true;
  networking.firewall.allowPing = true;
  services.openssh.enable = true;
  programs.fish.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGR4dqXwHwPpYgyk6yl9+9LRL3qrBZp3ZWdyKaTiXp0p vincent@shikoku"
  ];
  time.timeZone = "Europe/Paris";
}
