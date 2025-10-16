{
  lib,
  ...
}:
{
  console.keyMap = lib.mkForce "us";
  boot = {
    loader = {
      systemd-boot.enable = lib.mkForce false;
    };
  };
  boot.loader.raspberryPi.enable = true;
  # nothing ?
  # system.build.installBootLoader = config.boot.loader.raspberryPi;
}
