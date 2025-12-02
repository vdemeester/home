{ ... }:
{
  infrastructure.machine = {
    enable = true;
    hostname = "aix";

    network = {
      dnsNames = [
        "aix.vpn"
        "aix.sbr.pm"
      ];

      vpn = {
        enable = true;
        publicKey = "D1HoBqrqBchHOOi8mjKpVg5vZtt+iFy8wj4o3kGYwkc=";
        ips = [ "10.100.0.89" ];
      };
    };

    ssh = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEoUicDySCGETPAgmI0P3UrgZEXXw3zNsyCIylUP0bML";
    };

    syncthing = {
      enable = true;
      deviceID = "GHE6XF4-YCKEMZS-JEZYXA6-ETJI3SS-BQFFOCS-ZJAWN4D-Q33IQ46-OYL7BQM";
      folders = {
        sync = {
          type = "receiveonly";
        };
      };
    };
  };
}
