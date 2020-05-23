{ lib, pkgs, ... }:
{
  imports = [
    (import ../nix).home-manager
    ../hardware/thinkpad-x220.nix
    ../modules/module-list.nixos.nix
    # FIXME: remove this
    ../machines/home.nixos.nix
  ];

  networking = {
    hostName = "hokkaido";
  };

  # FIXME move this away
  home-manager.users.vincent = import ../home.nix;
  home-manager.users.root = { pkgs, ... }: {
    home.packages = with pkgs; [ htop ];
  };

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/884a3d57-f652-49b2-9c8b-f6eebd5edbeb";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/C036-34B9";
      fsType = "vfat";
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/e1833693-77ac-4d52-bcc7-54d082788639"; }];

  profiles = {
    avahi.enable = true;
    git.enable = true;
    ssh.enable = true;
    nix-config.buildCores = 2;
  };
}
