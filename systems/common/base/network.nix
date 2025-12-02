{ config, libx, ... }:
{
  # networking.extraHosts = ''
  #   10.100.0.80 nagoya.vpn
  # '';
  networking.hosts = libx.hostConfigs config.infrastructure.machines;
  # networking.hosts = {
  #   "192.168.1.80" = [ "nagoya.home" ];
  #   "10.100.0.80" = [ "nagoya.vpn" ];
  # };
}
