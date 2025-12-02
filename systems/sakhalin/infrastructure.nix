{ ... }:
{
  infrastructure.machine = {
    enable = true;
    hostname = "sakhalin";

    network = {
      localIPs = [ "192.168.1.70" ];
      dnsNames = [
        "sakhalin.home"
        "sakhalin.vpn"
        "sakhalin.sbr.pm"
      ];

      vpn = {
        enable = true;
        publicKey = "OAjw1l0z56F8kj++tqoasNHEMIWBEwis6iaWNAh1jlk=";
        ips = [ "10.100.0.16" ];
      };
    };

    ssh = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN/PMBThi4DhgZR8VywbRDzzMVh2Qp3T6NJAcPubfXz6";
    };

    syncthing = {
      enable = true;
      deviceID = "3L2KCXM-D75XCVU-5JLMV6V-FKQID2K-LJA6GFB-R2G77LD-5WXFHJT-BB4B7Q5";
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
