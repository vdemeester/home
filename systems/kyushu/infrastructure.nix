{ ... }:
{
  # Enable machine infrastructure configuration
  infrastructure.machine = {
    enable = true;
    hostname = "kyushu";

    network = {
      localIPs = [
        "192.168.1.36"
        "192.168.1.68"
      ];

      dnsNames = [
        "kyushu.home"
        "kyushu.vpn"
        "kyushu.sbr.pm"
      ];

      vpn = {
        enable = true;
        publicKey = "KVRzoPUw8UTQblYtbs/NLYLIVmtQehrc4Hacbpf5Ugs=";
        ips = [ "10.100.0.19" ];
      };
    };

    ssh = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINd795m+P54GlGJdMaGci9pQ9N942VUz8ri2F14+LWxg";
    };

    syncthing = {
      enable = true;
      deviceID = "SBLRZF4-NOMC7QO-S6UW7OH-VK7KHQS-LZCESY6-USBJ5Z5-RIVIRII-XS7DGQS";
      folders = {
        org = { };
        documents = { };
        sync = { };
        screenshots = { };
        wallpapers = { };
      };
    };
  };
}
