{
  libx,
  globals,
  lib,
  pkgs,
  ...
}:
{
  users.users.vincent.linger = true;

  services = {
    wireguard = {
      enable = true;
      ips = libx.wg-ips globals.machines.rhea.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
    # smartd = {
    #   enable = true;
    #   devices = [ { device = "/dev/nvme0n1"; } ];
    # };
    samba.settings = {
      settings = {
        "backup" = {
          path = "/neo/backup";
          public = true;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          writable = true;
          comment = "backup";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "vincent";
          "force group" = "users";
        };
        "documents" = {
          path = "/neo/documents";
          public = true;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          writable = true;
          comment = "documents";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "vincent";
          "force group" = "users";
        };
        "downloads" = {
          path = "/neo/downloads";
          public = true;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          writable = true;
          comment = "downloads";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "vincent";
          "force group" = "users";
        };
        "music" = {
          path = "/neo/music";
          public = true;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          writable = true;
          comment = "music";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "vincent";
          "force group" = "users";
        };
        "pictures" = {
          path = "/neo/pictures";
          public = true;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          writable = true;
          comment = "pictures";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "vincent";
          "force group" = "users";
        };
        "videos" = {
          path = "/neo/videos";
          public = true;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          writable = true;
          comment = "videos";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "vincent";
          "force group" = "users";
        };
      };
    };
    nfs.server = {
      enable = true;
      exports = ''
                /neo                      192.168.1.0/24(rw,fsid=0,no_subtree_check) 10.100.0.0/24(rw,fsid=0,no_subtree_check)
                /neo/backup               192.168.1.0/24(rw,fsid=1,no_subtree_check) 10.100.0.0/24(rw,fsid=1,no_subtree_check)
                /neo/documents            192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
                /neo/downloads            192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
                /neo/music                192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
                /neo/pictures             192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
                /neo/videos               192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
        			'';
    };
    # immich = {
    #   enable = true;
    #   user = "vincent";
    #   group = "users";
    #   mediaLocation = "/neo/pictures/photos";
    # };
    aria2 = {
      # FIXME: make sure aria2 runs as user vincent
      enable = true;
      openPorts = true;
      settings = {
        max-concurrent-downloads = 20;
        dir = "/neo/downloads";
      };
      rpcSecretFile = "${pkgs.writeText "aria" "aria2rpc\n"}"; # FIXME: use secrets for this somehow
    };
    transmission = {
      enable = true;
      user = "vincent";
      group = "users";
    };
    sonarr = {
      enable = true;
      user = "vincent";
      group = "users";
    };
    radarr = {
      enable = true;
      user = "vincent";
      group = "users";
    };
    bazarr = {
      enable = true;
      user = "vincent";
      group = "users";
    };
    prowlarr = {
      enable = true;
    };
    readarr = {
      enable = true;
      user = "vincent";
      group = "users";
    };
    lidarr = {
      enable = true;
      user = "vincent";
      group = "users";
    };
  };

  networking.useDHCP = lib.mkDefault true;

  environment.systemPackages = with pkgs; [
    lm_sensors
    gnumake
  ];

}
