{ config, pkgs, ... }: {
  imports = [
    ../hardware-configuration.nix
    ../networking.nix # generated at runtime by nixos-infect
    ../profiles/server.nix
    ../profiles/gitconfig.nix
    ../profiles/users.nix
    ../profiles/syncthing.nix
  ];

  boot.cleanTmpDir = true;
  boot.loader.grub.enable = true;
  networking.firewall.allowPing = true;
  services.openssh.enable = true;
  services.openssh.ports = with import ../assets/machines.nix; [ ssh.carthage.port ];
  services.openssh.permitRootLogin = "without-password";
  programs.fish.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGR4dqXwHwPpYgyk6yl9+9LRL3qrBZp3ZWdyKaTiXp0p vincent@shikoku"
  ];
  time.timeZone = "Europe/Paris";

  programs.podman.enable = true;

  services.syncthing-edge.guiAddress = with import ../assets/machines.nix; "${wireguard.ips.carthage}:8384";
  services.wireguard = with import ../assets/machines.nix; {
    enable = true;
    ips = [ "${wireguard.ips.carthage}/24" ];
    endpoint = wg.endpointIP;
    endpointPort = wg.listenPort;
    endpointPublicKey = wireguard.kerkouane.publicKey;
  };
}
