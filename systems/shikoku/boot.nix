{
  config,
  pkgs,
  ...
}:
{
  imports = [
    ../common/hardware/common-modules.nix
  ];

  # Enable common kernel modules
  boot.common-modules.enable = true;

  boot = {
    # supportedFilesystems = [ "zfs" ];
  };
}
