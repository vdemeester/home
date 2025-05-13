{ lib, ... }:
{
  net = {
    dns = {
      cacheNetworks = [ "192.168.1.0/24" "10.100.0.0/24" ];
      zones = [
        {
          # sbr
          name = "sbr.pm";
          master = true;
          slaves = [ ];
          file = ./secrets/db.sbr.pm;
        }
        {
          # home
          name = "home";
          master = true;
          slaves = [ ];
          file = ./secrets/db.home;
        }
        {
          # home.reverse
          name = "192.168.1.in-addr.arpa";
          master = true;
          slaves = [ ];
          file = ./secrets/db.192.168.1;
        }
        {
          # vpn
          name = "vpn";
          master = true;
          slaves = [ ];
          file = ./secrets/db.vpn;
        }
        {
          # vpn.reverse
          name = "10.100.0.in-addr.arpa";
          master = true;
          slaves = [ ];
          file = ./secrets/db.10.100.0;
        }
      ];
    };
    vpn = {
      endpoint = "167.99.17.238";
      pubkey = "+H3fxErP9HoFUrPgU19ra9+GDLQw+VwvLWx3lMct7QI=";
    };
  };
  machines = {
    athena = {
      net = {
        ips = [ "192.168.1.183" ];
        vpn = {
          pubkey = "RWqH7RdIXg+YE9U1nlsNiOC7jH8eWjWQmikqBVDGSXU=";
          ips = [ "10.100.0.83" ];
        };
      };
    };
    kyushu = {
      net = {
        ips = [
          "192.168.1.36"
          "192.168.1.68"
        ];
        vpn = {
          pubkey = "KVRzoPUw8UTQblYtbs/NLYLIVmtQehrc4Hacbpf5Ugs=";
          ips = [ "10.100.0.19" ];
        };
      };
      syncthing = {
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
          photos = {
            type = "receiveonly";
            paused = true; # TODO: implement this, start as paused
          };
          music = {
            type = "receiveonly";
            paused = true; # TODO: implement this, start as paused
          };
        };
      };
    };
  };
  # FIXME Maybe I should move this elsewhere, in ./lib maybe ?
  fn = {
    /**
         Return a list of wireguard ips from a list of ips.

         Essentially, it will append /24 to the each element of the list.
      *
    */
    wg-ips = ips: builtins.map (x: "${x}/24") ips;

    /**
        Return true if the given host has a list of Syncthing folder configured.
      *
    */
    hasSyncthingFolders =
      host:
      builtins.hasAttr "syncthing" host
      && builtins.hasAttr "folders" host.syncthing
      && (builtins.length (lib.attrsets.attrValues host.syncthing.folders)) > 0;
  };
}
