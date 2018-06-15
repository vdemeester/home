{ configs, pkgs, ... }:

{
  services = {
    avahi = {
      enable = true;
      ipv4 = true;
      ipv6 = true;
      nssmdns = true;
      publish = {
        enable = true;
        userServices = true;
      };
    };
  };
}
