{ lib, ... }:
{
  ssh = {
    vincent = [
      # Yubikeys
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBFT5Rx+4Wuvd8lMBkcHxb4oHdRhm/OTg+p5tvPzoIN9enSmgRw5Inm/SlS8ZzV87G1NESTgzDRi6hREvqDlKvxs="
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBGHMa4rHuBbQQYv+8jvlkFCD2VYRGA4+5fnZAhLx8iDirzfEPqHB60UJWcDeixnJCUlpJjzFbS4crNOXhfCTCTE="
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBBFzxC16VqwTgWDQfw2YCiOw2JzpH3z9XgHtKoHhBdHi2i9m9XUc7fIUeEIIf7P8ARRNd8q5bjvl8JY7LtPkNCU="
    ];
  };
  net = {
    dns = {
      cacheNetworks = [
        "192.168.1.0/24"
        "10.100.0.0/24"
      ];
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
      ssh = {
        root = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFQVlSrUKU0xlM9E+sJ8qgdgqCW6ePctEBD2Yf+OnyME root@aomi";
        vincent = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILJmTdMKYdgqpbQWBif58VBuwX+GqMGsMfB1ey1TKrM3 vincent@aomi";
      };
    };
    demeter = {
      net = {
        ips = [ "192.168.1.182" ];
        vpn = {
          pubkey = "/bBh4gvDty/AA2qIiHc7K0OHoOXWmj2SFFXdDq8nsUU=";
          ips = [ "10.100.0.82" ];
        };
      };
    };
    aix = {
      net = {
        vpn = {
          pubkey = "D1HoBqrqBchHOOi8mjKpVg5vZtt+iFy8wj4o3kGYwkc=";
          ips = [ "10.100.0.89" ];
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
    aomi = {
      net = {
        ips = [ "192.168.1.23" ];
        vpn = {
          pubkey = "XT4D9YLeVHwMb9R4mhBLSWHYF8iBO/UOT86MQL1jnA4=";
          ips = [ "10.100.0.17" ];
        };
      };
    };
    shikoku = {
      net = {
        ips = [ "192.168.1.24" ];
        vpn = {
          pubkey = "foUoAvJXGyFV4pfEE6ISwivAgXpmYmHwpGq6X+HN+yA=";
          ips = [ "10.100.0.2" ];
        };
      };
      ssh = {
        vincent = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGxstR3xEf87leVVDS3GVPx8Ap9+eP+OfkSvM26V54XP vincent@shikoku";
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

    # # keysFor = user: ;
    # hasSSHAttr = _name: value: builtins.hasAttr "ssh" value;
    # keysFor =
    #   machines: user:
    #   lib.attrsets.mapAttrsToList (_name: value: value) (lib.attrsets.filterAttrs hasSSHAttr machines);

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
