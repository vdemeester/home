{ hostname, lib, ... }:
let

  isCurrentHost = n: n == hostname;
  hasVPNPublicKey = host: (lib.attrsets.attrByPath [ "net" "vpn" "pubkey" ] "" host) != "";
  hasVPNips = host: (builtins.length (lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ ] host)) > 0;
  /**
      Return true if the given host has a list of Syncthing folder configured.
    *
  */
  hasSyncthingFolders =
    host:
    builtins.hasAttr "syncthing" host
    && builtins.hasAttr "folders" host.syncthing
    && (builtins.length (lib.attrsets.attrValues host.syncthing.folders)) > 0;

  # Get the path for the given folder, either using the host specified path or the default one
  syncthingFolderPath =
    name: folder: folders:
    lib.attrsets.attrByPath [ "path" ] folders."${name}".path folder;

  # Filter machine with the given syncthing folder
  syncthingMachinesWithFolder =
    folderName: machines:
    lib.attrsets.filterAttrs (
      name: value:
      hasSyncthingFolders value
      && !(isCurrentHost name)
      && (builtins.hasAttr folderName value.syncthing.folders)
    ) machines;

  generateSyncthingAdresses =
    machine:
    builtins.map (x: "tcp://${x}") (
      lib.attrsets.attrByPath [ "net" "ips" ] [ ] machine
      ++ lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ ] machine
      ++ lib.attrsets.attrByPath [ "net" "names" ] [ ] machine
    );
in
{
  ssh = {
    vincent = [
      # Yubikeys
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBFT5Rx+4Wuvd8lMBkcHxb4oHdRhm/OTg+p5tvPzoIN9enSmgRw5Inm/SlS8ZzV87G1NESTgzDRi6hREvqDlKvxs="
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBGHMa4rHuBbQQYv+8jvlkFCD2VYRGA4+5fnZAhLx8iDirzfEPqHB60UJWcDeixnJCUlpJjzFbS4crNOXhfCTCTE="
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBBFzxC16VqwTgWDQfw2YCiOw2JzpH3z9XgHtKoHhBdHi2i9m9XUc7fIUeEIIf7P8ARRNd8q5bjvl8JY7LtPkNCU="
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILJmTdMKYdgqpbQWBif58VBuwX+GqMGsMfB1ey1TKrM3 vincent@aomi"
    ];
  };
  syncthingFolders = {
    sync = {
      id = "7dshg-r8zr6";
      path = "/home/vincent/sync";
    };
    documents = {
      id = "oftdb-t5anv";
      path = "/home/vincent/desktop/documents";
    };
    org = {
      id = "sjpsr-xfwdu";
      path = "/home/vincent/desktop/org";
    };
    screenshots = {
      id = "prpsz-azlz9";
      path = "/home/vincent/desktop/pictures/screenshots";
    };
    wallpapers = {
      id = "wpiah-ydwwx";
      path = "/home/vincent/desktop/pictures/wallpapers";
    };
    photos = {
      id = "uetya-ypa3d";
      path = "/home/vincent/desktop/pictures/photos";
    };
    music = {
      id = "kcyrf-mugzt";
      path = "/home/vincent/desktop/music";
    };
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
        names = [
          "ahena.home"
          "athena.vpn"
          "athena.sbr.pm"
        ];
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
        names = [
          "demeter.home"
          "demeter.vpn"
          "demeter.sbr.pm"
        ];
      };
    };
    aix = {
      net = {
        vpn = {
          pubkey = "D1HoBqrqBchHOOi8mjKpVg5vZtt+iFy8wj4o3kGYwkc=";
          ips = [ "10.100.0.89" ];
        };
      };
      # syncthing = {
      # 	folders = {
      # 		sync = {
      #       type = "receiveonly";};
      # 	};
      # };
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
        names = [
          "kyushu.home"
          "kyushu.vpn"
          "kyushu.sbr.pm"
        ];
      };
      syncthing = {
        id = "SBLRZF4-NOMC7QO-S6UW7OH-VK7KHQS-LZCESY6-USBJ5Z5-RIVIRII-XS7DGQS";
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
          # TODO: implement paused or filter theses
          # photos = {
          #   type = "receiveonly";
          #   paused = true; # TODO: implement this, start as paused
          # };
          # music = {
          #   type = "receiveonly";
          #   paused = true; # TODO: implement this, start as paused
          # };
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
        names = [
          "aomi.home"
          "aomi.vpn"
          "aomi.sbr.pm"
        ];
      };
      syncthing = {
        id = "XCR6WWB-OZUDGFB-LQPFW73-MV5SPJK-4IGOMA4-IAXON3I-C6OFETL-TPK5FQS";
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
          # photos = {
          #   type = "receiveonly";
          #   paused = true; # TODO: implement this, start as paused
          # };
        };
      };
    };
    shikoku = {
      net = {
        ips = [ "192.168.1.24" ];
        vpn = {
          pubkey = "KVRzoPUw8UTQblYtbs/NLYLIVmtQehrc4Hacbpf5Ugs=";
          ips = [ "10.100.0.2" ];
        };
        names = [
          "shikoku.home"
          "shikoku.vpn"
          "shikoku.sbr.pm"
        ];
      };
      syncthing = {
        id = "KZMMXRR-UINDQTS-H3TV2W7-EIGOUDI-3LW4ZDG-7PRKDFV-MJ5KUTJ-YG5Y5AI";
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
        };
      };
      ssh = {
        vincent = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGxstR3xEf87leVVDS3GVPx8Ap9+eP+OfkSvM26V54XP vincent@shikoku";
      };
    };
    kerkouane = {
      net = {
        vpn = {
          pubkey = "+H3fxErP9HoFUrPgU19ra9+GDLQw+VwvLWx3lMct7QI=";
          ips = [ "10.100.0.1" ];
        };
        names = [
          "kerkouame.home"
          "kerkouane.vpn"
          "kerkouane.sbr.pm"
        ];
      };
      syncthing = {
        id = "IFVRRQ7-KMIOQXP-5YDJXQU-UJXUKHB-7THCSY6-B3NHRNA-ED7IRI7-2JPPKQY";
        folders = {
          sync = { };
          org = { };
        };
      };
    };
    sakhalin = {
      net = {
        ips = [ "192.168.1.70" ];
        vpn = {
          pubkey = "OAjw1l0z56F8kj++tqoasNHEMIWBEwis6iaWNAh1jlk=";
          ips = [ "10.100.0.16" ];
        };
        names = [
          "sakhalin.home"
          "sakhalin.vpn"
          "sakhalin.sbr.pm"
        ];
      };
      syncthing = {
        id = "4TYYG7V-A67D5SN-HMEJCI7-POOZRLL-RNCIE4U-ZYVGTOB-JQ5DOSV-ZCGWUAL";
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
          # photos = {
          #   type = "receiveonly";
          #   paused = true; # TODO: implement this, start as paused
          # };
        };
      };
    };
    kobe = {
      net = {
        ips = [ "192.168.1.77" ];
        vpn = {
          pubkey = "B9jLGtXGZEfvBrgyEKrFRrsCsTsarfpFeyXqqq1NOWg=";
          ips = [ "10.100.0.77" ];
        };
        names = [
          "kobe.home"
          "kobe.vpn"
          "kobe.sbr.pm"
        ];
      };
      syncthing = {
        id = "";
        folders = {
          org = { };
          documents = { };
          sync = { };
        };
      };
    };
    aion = {
      net = {
        ips = [ "192.168.1.49" ];
        vpn = {
          pubkey = "T8qfsBiOcZNxUeRHFg+2FPdGj4AuGloJ4b+0uI2jM2w=";
          ips = [ "10.100.0.49" ];
        };
        names = [
          "aion.home"
          "aion.vpn"
          "aion.sbr.pm"
        ];
      };
      syncthing = {
        id = "YORNSGU-UC4IAG5-IWJCD7T-MVPIU7O-AYM36UK-LEHF7AP-CBC4L6C-ZWKUYQF";
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
          # photos = {
          #   paused = true; # TODO: implement this, start as paused
          # };
          # music = {
          #   paused = true; # TODO: implement this, start as paused
          # };
        };
      };
    };
    okinawa = {
      net = {
        ips = [ "192.168.1.19" ];
        vpn = {
          # pubkey = "";
          ips = [ "10.100.0.14" ];
        };
      };
      syncthing = {
        id = "2RWT47Z-UGSH4QO-G4W6XN7-3XY722R-ZKGDN5U-4MDGHMA-6SM26QM-7VCQIAZ";
        folders = {
          sync = { };
          org = { };
        };
      };
    };
    # iPhone
    hokkaido = {
      net = {
        ips = [ "192.168.1.115" ];
        vpn = {
          pubkey = "1wzFG60hlrAoSYcRKApsH+ZK3Zyz8ljdLglb/8JbuW0=";
          ips = [ "10.100.0.5" ];
        };
      };
      syncthing = {
        id = "XD4XYNZ-DT3PJEY-UJYBHWX-6OQPPUI-HTW752L-FYTX3TW-GVHDTKW-PT336QV";
        folders = {
          documents = { };
          sync = { };
          org = { };
        };
      };
    };
    # Light Phone
    Suzu = {
      net = {
        vpn = {
          ips = [ "10.100.0.65" ];
          pubkey = "ufKLXzLkmYx1z7/VZJs9Ix6aXL3rYzP5B73QQP2WNx8=";
        };
      };
    };
    # Boox tablet
    Osaka = {
      net = {
        vpn = {
          ips = [ "10.100.0.64" ];
          pubkey = "C12Ch3LasZ9Dvc1+X+IMSmKdip0l1n/aNNPvmQzzPFY=";
        };
      };
    };
  };
  # FIXME Maybe I should move this elsewhere, in ./lib maybe ?
  fn = {
    inherit
      syncthingFolderPath
      hasSyncthingFolders
      syncthingMachinesWithFolder
      generateSyncthingAdresses
      isCurrentHost
      hasVPNPublicKey
      hasVPNips
      ;
    /**
         Return a list of wireguard ips from a list of ips.

         Essentially, it will append /32 to the each element of the list.
      *
    */
    wg-ips = ips: builtins.map (x: "${x}/32") ips;

    # # keysFor = user: ;
    # hasSSHAttr = _name: value: builtins.hasAttr "ssh" value;
    # keysFor =
    #   machines: user:
    #   lib.attrsets.mapAttrsToList (_name: value: value) (lib.attrsets.filterAttrs hasSSHAttr machines);

    # WIREGUARD
    generateWireguardPeers =
      machines:
      lib.attrsets.attrValues (
        lib.attrsets.mapAttrs
          (_name: value: {
            allowedIPs = value.net.vpn.ips;
            publicKey = value.net.vpn.pubkey;
          })
          (
            lib.attrsets.filterAttrs (
              name: value: name != "kerkouane" && (hasVPNPublicKey value) && (hasVPNips value)
            ) machines
          )
      );

    # SYNCTHING
    generateSyncthingFolders =
      machine: machines: folders:
      lib.attrsets.mapAttrs' (
        name: value:
        lib.attrsets.nameValuePair (syncthingFolderPath name value folders) {
          inherit (folders."${name}") id;
          label = name;
          devices = lib.attrsets.mapAttrsToList (n: _v: n) (syncthingMachinesWithFolder name machines);
          rescanIntervalS = 3600 * 6; # TODO: make it configurable
        }
      ) (lib.attrsets.attrByPath [ "syncthing" "folders" ] { } machine);

    generateSyncthingDevices =
      machines:
      lib.attrsets.mapAttrs
        (_name: value: {
          inherit (value.syncthing) id;
          addresses = generateSyncthingAdresses value;
        })
        (
          lib.attrsets.filterAttrs (name: value: hasSyncthingFolders value && !(isCurrentHost name)) machines
        );

    syncthingGuiAddress =
      machine:
      (builtins.head (lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ "127.0.0.1" ] machine)) + ":8384";
  };
}
