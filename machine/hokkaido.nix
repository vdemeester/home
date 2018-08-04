{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
    ../hardware-configuration.nix
    ../profiles/laptop.nix
    ../profiles/ssh.nix
    ../profiles/yubikey.nix
    ../profiles/dev.nix
    #../profiles/containerd.nix
    #../profiles/dockerization.nix
    #../profiles/virtualization.nix
    ../profiles/wireguard.nix
    ../location/home.nix
    ../hardware/thinkpad-x220.nix
  ];

  boot.loader.efi.canTouchEfiVariables = true;
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  hardware.trackpoint.enable = false;
  time.timeZone = "Europe/Paris";

  services.xserver.displayManager.slim.theme = pkgs.fetchurl {
    url = "https://github.com/vdemeester/slim-themes/raw/master/docker-key-theme-0.1.tar.xz";
    sha256 = "127893l1nzqya0g68k8841g5lm3hlnx7b3b3h06axvplc54a1jd8";
  };
}
