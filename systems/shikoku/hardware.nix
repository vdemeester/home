{
  config,
  ...
}:
{
  networking = {
    hostId = builtins.substring 0 8 (builtins.hashString "md5" config.networking.hostName);
    # Bridge setup
    bridges.br1.interfaces = [ "enp0s31f6" ];
    useDHCP = false;
    interfaces.br1 = {
      useDHCP = true;
    };
    # FIXME probably change this
    firewall.enable = false; # we are in safe territory :D
  };
  hardware.nvidia = {
    modesetting.enable = true;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };
  hardware.graphics = {
    enable = true;
  };
  nixpkgs.config.allowUnfree = true;
}
