{ desktop
, pkgs
, self
, ...
}:
{
  imports = [
    # (./. + "/${desktop}.nix")

    ./binfmt.nix
    # ../hardware/yubikey.nix
    # ../services/pipewire.nixt
    # ../virt
  ];
  # Enable Plymouth and surpress some logs by default.
  boot = {
    plymouth.enable = true;
    kernelParams = [
      # The 'splash' arg is included by the plymouth option
      "quiet"
      "loglevel=3"
      "rd.udev.log_priority=3"
      "vt.global_cursor_default=0"
    ];
  };

  hardware.graphics.enable = true;

  # Enable location services
  location.provider = "geoclue2";

  environment.systemPackages = with pkgs; [
    bat # cat
    cyme # lsusb
    dig
    eza # ls
    fd # find
    duf # df
    ripgrep
  ];

  services.power-profiles-daemon.enable = true;
}
