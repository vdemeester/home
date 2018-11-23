{ config, pkgs, ... }:

{
  imports = [
    ../location/home.nix
  ];

  profiles.desktop.enable = true;
  profiles.docker.enable = true;
  profiles.buildkit.enable = true;
  profiles.containerd.enable = true;
  profiles.virtualization.enable = true;
  profiles.ssh.enable = true;
  profiles.dev.enable = true;
  profiles.gaming.enable = true;
  
  time.timeZone = "Europe/Paris";

  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.efiSupport = true;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.grub.devices = ["nodev"];
  boot.loader.grub.extraEntries = ''
        menuentry "Windows" {
          insmod part_gpt
          insmod fat
          insmod search_fs_uuid
          insmod chain
          search --fs-uuid --no-floppy --set=root 122F-2055
          chainloader ($root)/Microsoft/Boot/bootmgfw.efi
        }
      '';
  boot.loader.grub.useOSProber = true;
  
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
  networking.firewall.allowedUDPPortRanges = [ { from = 6001; to = 6101; } ];
  networking.firewall.allowedTCPPorts = [ 7946 9000 5000 ];

  # Move elsewhere
  programs.podman = {
    enable = true;
  };

  services.syncthing-edge.guiAddress = with import ../assets/machines.nix; "${wireguard.ips.shikoku}:8384";
  services.wireguard = with import ../assets/machines.nix; {
    enable = true;
    ips = [ "${wireguard.ips.shikoku}/24" ];
    endpoint = wg.endpointIP;
    endpointPort = wg.listenPort;
    endpointPublicKey = wireguard.kerkouane.publicKey;
  };
}
