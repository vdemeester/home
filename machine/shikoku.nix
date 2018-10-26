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
    ../location/home.nix
  ];

  time.timeZone = "Europe/Paris";
  #boot.loader.systemd-boot.enable = true;
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.efiSupport = true;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.grub.devices = ["nodev"];
  #boot.loader.grub.device = "/dev/nvme0n1";
  boot.loader.grub.extraEntries = ''
        menuentry "Windows" {
          insmod part_gpt
          insmod fat
          insmod search_fs_uuid
          insmod chain
          search --fs--uid --set=root $FS_UUID
          chainloader /EFI/Microsoft/Boot/bootmgfw.efi
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

   services.wireguard = with import ../assets/machines.nix; {
    enable = true;
    ips = [ "${wireguard.ips.shikoku}/24" ];
    endpoint = wg.endpointIP;
    endpointPort = wg.listenPort;
    endpointPublicKey = wireguard.kerkouane.publicKey;
  };
}
