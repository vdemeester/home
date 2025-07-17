{
  config,
  lib,
  pkgs,
  ...
}:
{
  console.keyMap = lib.mkForce "us";
  boot = {
    loader = {
      systemd-boot.enable = lib.mkForce false;
    };
  };
  # nothing ?
  # system.build.installBootLoader = config.boot.loader.raspberryPi;
}
