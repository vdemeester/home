{ pkgs, lib, ... }:
#let
#  sources = import ../../nix/sources.nix;
#in
{
  imports = [
    # (sources.nixos-hardware + "/common/pc/ssd")
    ./thinkpad.nix
  ];
  boot = {
    initrd.availableKernelModules = [
      "nvme"
      "rtsx_pci_sdmmc"
    ];
  };
  hardware = {

    enableAllFirmware = true;
    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
    nvidia.prime = {
      sync.enable = true;
      nvidiaBusId = "PCI:1:0:0";
      intelBusId = "PCI:0:2:0";
    };
  };
  nix.maxJobs = 12;
  services.throttled.enable = lib.mkDefault true;
  services = {
    tlp = {
      settings = {
        # CPU optimizations
        "CPU_SCALING_GOVERNOR_ON_AC" = "performance";
        "CPU_SCALING_GOVERNOR_ON_BAT" = "powersave";
        "CPU_MIN_PERF_ON_AC" = 0;
        "CPU_MAX_PERF_ON_AC" = 100;
        "CPU_MIN_PERF_ON_BAT" = 0;
        "CPU_MAX_PERF_ON_BAT" = 50;
        # DEVICES (wifi, ..)
        "DEVICES_TO_DISABLE_ON_STARTUP" = "";
        "DEVICES_TO_ENABLE_ON_AC" = "bluetooth wifi wwan";
        "DEVICES_TO_DISABLE_ON_BAT" = "";
        # Network management
        "DEVICES_TO_DISABLE_ON_LAN_CONNECT" = "";
        "DEVICES_TO_DISABLE_ON_WIFI_CONNECT" = "";
        "DEVICES_TO_DISABLE_ON_WWAN_CONNECT" = "";
        "DEVICES_TO_ENABLE_ON_LAN_DISCONNECT" = "";
        "DEVICES_TO_ENABLE_ON_WIFI_DISCONNECT" = "";
        "DEVICES_TO_ENABLE_ON_WWAN_DISCONNECT" = "";
        # Docking
        "DEVICES_TO_DISABLE_ON_DOCK" = "wifi";
        "DEVICES_TO_ENABLE_ON_UNDOCK" = "wifi";
        # Make sure it uses the right hard drive
        "DISK_DEVICES" = "nvme0n1p2";
      };
    };
    udev.extraRules = ''
      # Rules for Lenovo Thinkpad WS Dock
      SUBSYSTEM=="usb", ACTION=="add|remove", ENV{ID_VENDOR}=="17ef", ENV{ID_MODEL}=="305a", RUN+="${pkgs.vde-thinkpad}/bin/dock"
      ACTION=="add", SUBSYSTEM=="thunderbolt", ATTR{authorized}=="0", ATTR{authorized}="1"
    '';
  };
}
