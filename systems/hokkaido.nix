{ lib, pkgs, ... }:
let
  inCi = builtins.pathExists /home/build;
  enableHome = !inCi;
in
{
  imports = [
    # hardware
    ../hardware/thinkpad-x220.nix
    # modules
    ../modules
    # users
    (import ../users).vincent
    (import ../users).root
  ];

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

  networking = {
    hostName = "hokkaido";
  };

  profiles = {
    home = enableHome;
    laptop.enable = true;
    avahi.enable = true;
    git.enable = true;
    ssh.enable = true;
    users.enable = false;
    users.withMachines = enableHome;
    mail.enable = enableHome;
  };
}
