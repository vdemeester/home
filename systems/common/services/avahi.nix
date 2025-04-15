{ desktop, ... }:
{
  services = {
    avahi = {
      enable = true;
      openFirewall = true;
      nssmdns4 = true;
      ipv4 = true;
      ipv6 = true;
      publish = {
        enable = true;
        userServices = true;
        addresses = true;
        workstation = if (builtins.isString desktop) then true else false;
      };
    };
  };
}
