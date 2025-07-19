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

  hasSSHHostKeys = host: builtins.hasAttr "ssh" host && builtins.hasAttr "hostKey" host.ssh;

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

  sshHostIdentifier =
    machine:
    lib.attrsets.attrByPath [ "net" "names" ] [ ] machine
    ++ lib.attrsets.attrByPath [ "net" "ips" ] [ ] machine
    ++ lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ ] machine;

  hostConfig =
    machine:
    builtins.listToAttrs (
      map
        (x: {
          name = x;
          value =
            if (lib.strings.hasPrefix "10.100" x) then
              builtins.filter (n: lib.strings.hasSuffix ".vpn" n) machine.net.names
            else if (lib.strings.hasPrefix "192.168" x) then
              builtins.filter (n: lib.strings.hasSuffix ".home" n) machine.net.names
            else
              [ ];
        })
        (
          lib.attrsets.attrByPath [ "net" "ips" ] [ ] machine
          ++ lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ ] machine
        )
    );

  sshConfig =
    machine:
    builtins.listToAttrs (
      map
        (x: {
          name = x;
          value = {
            hostname =
              if (lib.strings.hasSuffix ".vpn" x) then
                builtins.head machine.net.vpn.ips
              else if (lib.strings.hasSuffix ".home" x) then
                builtins.head machine.net.ips
              else
                x;
            forwardAgent = true;
          };
        })
        (
          builtins.filter (x: (lib.strings.hasSuffix ".home" x) || (lib.strings.hasSuffix ".vpn" x)) (
            sshHostIdentifier machine
          )
        )
    );
in
{
  ssh = {
    vincent = [
      # Yubikeys
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBFT5Rx+4Wuvd8lMBkcHxb4oHdRhm/OTg+p5tvPzoIN9enSmgRw5Inm/SlS8ZzV87G1NESTgzDRi6hREvqDlKvxs="
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBGHMa4rHuBbQQYv+8jvlkFCD2VYRGA4+5fnZAhLx8iDirzfEPqHB60UJWcDeixnJCUlpJjzFbS4crNOXhfCTCTE="
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBBFzxC16VqwTgWDQfw2YCiOw2JzpH3z9XgHtKoHhBdHi2i9m9XUc7fIUeEIIf7P8ARRNd8q5bjvl8JY7LtPkNCU="
      # AOMI (only "trusted" one)
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
          "athena.home"
          "athena.vpn"
          "athena.sbr.pm"
        ];
      };
      ssh = {
        hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM/4KRP1rzOwyA2zP1Nf1WlLRHqAGutLtOHYWfH732xh";
        # root = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFQVlSrUKU0xlM9E+sJ8qgdgqCW6ePctEBD2Yf+OnyME root@aomi";
        # vincent = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILJmTdMKYdgqpbQWBif58VBuwX+GqMGsMfB1ey1TKrM3 vincent@aomi";
      };
      syncthing = {
        id = "N3AMUVI-FM2BAOD-U3OMZDJ-UHMQE6J-ACMM5B7-S7BTK6P-PSM36NR-DVZHLQF";
        folders = {
          org = {
            type = "receiveonly";
          };
          sync = {
            type = "receiveonly";
          };
        };
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
      ssh = {
        hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGqQfEyHyjIGglayB9FtCqL7bnYfNSQlBXks2IuyCPmd";
      };
      syncthing = {
        id = "TXCV3TS-TUEOTH6-ETB3LBV-KCIHT4L-RCCOIE3-VPBCNJB-VHQEAYI-WOXK5A5";
        folders = {
          org = {
            type = "receiveonly";
          };
          sync = {
            type = "receiveonly";
          };
        };
      };
    };
    nagoya = {
      net = {
        ips = [ "192.168.1.80" ];
        vpn = {
          pubkey = "u2K7SAZpeBACUMkoscyyNFjJdDnJcpbW9YHr3HewFhM=";
          ips = [ "10.100.0.80" ];
        };
        names = [
          "nagoya.home"
          "nagoya.vpn"
          "nagoya.sbr.pm"
        ];
      };
      ssh = {
        hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHeP9q2U/eS/GijfUtqLRENHSRPFSo1tN1iorIj1b0+O";
      };
      syncthing = {
        id = "7BGILOF-DUA47DF-7S5732W-X22RNOD-UNOLDX2-NTCKNR4-LR64Q5A-Y5LWMA6";
        folders = {
          sync = {
            type = "receiveonly";
          };
        };
      };
    };
    aix = {
      net = {
        vpn = {
          pubkey = "D1HoBqrqBchHOOi8mjKpVg5vZtt+iFy8wj4o3kGYwkc=";
          ips = [ "10.100.0.89" ];
        };
        names = [
          "aix.vpn"
          "aix.sbr.pm"
        ];
      };
      syncthing = {
        id = "GHE6XF4-YCKEMZS-JEZYXA6-ETJI3SS-BQFFOCS-ZJAWN4D-Q33IQ46-OYL7BQM";
        folders = {
          sync = {
            type = "receiveonly";
          };
        };
      };
      ssh = {
        hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEoUicDySCGETPAgmI0P3UrgZEXXw3zNsyCIylUP0bML";
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
        names = [
          "kyushu.home"
          "kyushu.vpn"
          "kyushu.sbr.pm"
        ];
      };
      ssh = {
        hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINd795m+P54GlGJdMaGci9pQ9N942VUz8ri2F14+LWxg";
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
      ssh = {
        hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFQVlSrUKU0xlM9E+sJ8qgdgqCW6ePctEBD2Yf+OnyME";
      };
      syncthing = {
        id = "CN5P3MV-EJ65J4I-OHB7OBI-LD7JBWT-7SZCZD3-Z6NAASI-UCMKOAU-X2TNNAP";
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
      ssh = {
        hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH18c6kcorVbK2TwCgdewL6nQf29Cd5BVTeq8nRYUigm";
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
          "kerkouane.vpn"
          "kerkouane.sbr.pm"
        ];
      };
      ssh = {
        hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJguVoQYObRLyNxELFc3ai2yDJ25+naiM3tKrBGuxwwA";
      };
      syncthing = {
        id = "QGD6ICB-EPSGCEN-IQWKN77-BCRWE67-56HX5IA-E4IDBCI-WE46DK3-EC63DQ7";
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
      ssh = {
        hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN/PMBThi4DhgZR8VywbRDzzMVh2Qp3T6NJAcPubfXz6";
      };
      syncthing = {
        id = "3L2KCXM-D75XCVU-5JLMV6V-FKQID2K-LJA6GFB-R2G77LD-5WXFHJT-BB4B7Q5";
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
      ssh = {
        # hostKey = "";
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
      ssh = {
        hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMs2o62unBFN/LHRg3q2N4QyZW0+DC/gjw3yzRbWdzx5";
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
    synodine = {
      net = {
        ips = [ "192.168.1.20" ];
        names = [
          "synodine.home"
          "synodine.sbr.pm"
        ];
      };
      ssh = {
        hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDWdnPJg0Y4kd4lHPAGE4xgMAK2qvMg3oBxh0t+xO+7O";
      };
    };
    okinawa = {
      net = {
        ips = [ "192.168.1.19" ];
        vpn = {
          # pubkey = "";
          ips = [ "10.100.0.14" ];
        };
        names = [
          "okinawa.home"
          "okinawa.vpn"
          "okinawa.sbr.pm"
        ];
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
          pubkey = "1wzFG60hlrAoSYcRKApsH+WK3Zyz8ljdLglb/8JbuW0=";
          ips = [ "10.100.0.5" ];
        };
        names = [
          "hokkaido.home"
          "hokkaido.vpn"
          "hokkaido.sbr.pm"
        ];
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
    suzu = {
      net = {
        vpn = {
          ips = [ "10.100.0.65" ];
          pubkey = "ufKLXzLkmYx1z7/VZJs9Ix6aXL3rYzP5B73QQP2WNx8=";
        };
        names = [
          # "suzu.home"
          "suzu.vpn"
          "suzu.sbr.pm"
        ];
      };
    };
    # Boox tablet
    osaka = {
      net = {
        vpn = {
          ips = [ "10.100.0.64" ];
          pubkey = "C12Ch3LasZ9Dvc1+X+IMSmKdip0l1n/aNNPvmQzzPFY=";
        };
        names = [
          # "oksaka.home"
          "osaka.vpn"
          "osaka.sbr.pm"
        ];
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
      hasSSHHostKeys
      sshHostIdentifier
      sshConfig
      hostConfig
      ;
    /**
         Return a list of wireguard ips from a list of ips.

         Essentially, it will append /32 to the each element of the list.
      *
    */
    wg-ips = ips: builtins.map (x: "${x}/32") ips;

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

    # SSH

    sshKnownHosts =
      machines:
      lib.strings.concatStringsSep "\n" (
        lib.attrsets.mapAttrsToList (
          _name: value: "${lib.strings.concatStringsSep "," (sshHostIdentifier value)} ${value.ssh.hostKey}"
        ) (lib.attrsets.filterAttrs (_name: hasSSHHostKeys) machines)
      );

    hostConfigs =
      machines: lib.attrsets.mergeAttrsList (lib.attrsets.mapAttrsToList (_name: hostConfig) (machines));

    sshConfigs =
      machines:
      lib.attrsets.mergeAttrsList (
        lib.attrsets.mapAttrsToList (_name: sshConfig) (
          lib.attrsets.filterAttrs (_name: _value: true) machines
        )
      );
  };
}
