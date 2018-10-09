{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ../hardware-configuration.nix
    ../profiles/desktop.nix
    ../profiles/ssh.nix
    ../profiles/audio.nix
    ../profiles/dev.nix
    ../profiles/buildkitd.nix
    ../profiles/containerd.nix
    ../profiles/virtualization.nix
    ../profiles/dockerization.nix
    ../profiles/gaming.nix
    ../profiles/wireguard.nix
    ../service/wireguard.client.nix
    ../location/home.nix
  ];

  time.timeZone = "Europe/Paris";
  boot.loader.systemd-boot.enable = true;

  services = {
    xserver = {
      videoDrivers = [ "nvidia" ];
      displayManager = {
        sessionCommands = ''
          xrandr --output DP-2 --auto --left-of DP-4 --output DP-4 --auto &
        '';
        slim.theme = pkgs.fetchurl {
          url = "https://github.com/vdemeester/slim-themes/raw/master/docker-nuage-theme-0.1.tar.xz";
          sha256 = "1ds7p3d8dn21bankgs68i53hqrj4d2abpk437h6dbjz36q1ys839";
        };
      };
    };
  };

  hardware.bluetooth.enable = true;
  networking.firewall.allowedTCPPorts = [ 7946 9000 ];

  services.wireguard = with import ../assets/wireguard.nix; {
    enable = true;
    ips = [ "${ips.shikoku}/24" ];
    endpoint = main.endpointIP;
    endpointPort = main.listenPort;
    endpointPublicKey = kerkouane.publicKey;
  };
}
