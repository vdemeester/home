{ lib, ... }:
{
  # This module provides a registry of all machines for cross-machine references
  # Used primarily by DNS zone generation and syncthing configuration
  options.infrastructure.machines = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          net = {
            ips = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
            };
            vpn = {
              pubkey = lib.mkOption {
                type = lib.types.nullOr lib.types.str;
                default = null;
              };
              ips = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
              };
            };
            names = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
            };
          };
          ssh = {
            hostKey = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
            };
            vincent = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              description = "Vincent's SSH key on this machine (if different from standard keys)";
            };
          };
          syncthing = {
            id = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
            };
            folders = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
          };
        };
      }
    );
    default = { };
    description = "Registry of all machines in the infrastructure";
  };

  # Pre-populate the registry with all known machines
  # This maintains backward compatibility with DNS zone files
  config.infrastructure.machines = {
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
      ssh.hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM/4KRP1rzOwyA2zP1Nf1WlLRHqAGutLtOHYWfH732xh";
      syncthing = {
        id = "N3AMUVI-FM2BAOD-U3OMZDJ-UHMQE6J-ACMM5B7-S7BTK6P-PSM36NR-DVZHLQF";
        folders.sync.type = "receiveonly";
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
      ssh.hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGqQfEyHyjIGglayB9FtCqL7bnYfNSQlBXks2IuyCPmd";
      syncthing = {
        id = "TXCV3TS-TUEOTH6-ETB3LBV-KCIHT4L-RCCOIE3-VPBCNJB-VHQEAYI-WOXK5A5";
        folders.sync.type = "receiveonly";
      };
    };
    nagoya = {
      net = {
        ips = [ "192.168.1.80" ];
        vpn = {
          pubkey = "NCj5pwShre/xyRCK800ybjso1zIYUZ08YvFA2qGzhAI=";
          ips = [ "10.100.0.80" ];
        };
        names = [
          "nagoya.home"
          "nagoya.vpn"
          "nagoya.sbr.pm"
        ];
      };
      ssh.hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIfep1SkMsAPHggXFLfEJNzZb7eoihtkqDeQruG+TbhF";
      syncthing = {
        id = "HZDLS5A-LKCEIYQ-DDMDYDF-DBTSRYH-HUNQSII-TVCDACT-DIYIO7V-G4K2EQV";
        folders.sync.type = "receiveonly";
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
      ssh.hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEoUicDySCGETPAgmI0P3UrgZEXXw3zNsyCIylUP0bML";
      syncthing = {
        id = "GHE6XF4-YCKEMZS-JEZYXA6-ETJI3SS-BQFFOCS-ZJAWN4D-Q33IQ46-OYL7BQM";
        folders.sync.type = "receiveonly";
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
      ssh.hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINd795m+P54GlGJdMaGci9pQ9N942VUz8ri2F14+LWxg";
      syncthing = {
        id = "SBLRZF4-NOMC7QO-S6UW7OH-VK7KHQS-LZCESY6-USBJ5Z5-RIVIRII-XS7DGQS";
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
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
      ssh.hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFQVlSrUKU0xlM9E+sJ8qgdgqCW6ePctEBD2Yf+OnyME";
      syncthing = {
        id = "CN5P3MV-EJ65J4I-OHB7OBI-LD7JBWT-7SZCZD3-Z6NAASI-UCMKOAU-X2TNNAP";
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
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
        names = [
          "shikoku.home"
          "shikoku.vpn"
          "shikoku.sbr.pm"
        ];
      };
      ssh = {
        hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH18c6kcorVbK2TwCgdewL6nQf29Cd5BVTeq8nRYUigm";
        vincent = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGxstR3xEf87leVVDS3GVPx8Ap9+eP+OfkSvM26V54XP vincent@shikoku";
      };
      syncthing = {
        id = "ZKUNKBI-N2K2LTG-AWLDAEX-NE6NALQ-DLFO6YV-FU4A7IE-KCF5ZCD-IEYSKAH";
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
        };
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
      ssh.hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJguVoQYObRLyNxELFc3ai2yDJ25+naiM3tKrBGuxwwA";
      syncthing = {
        id = "QGD6ICB-EPSGCEN-IQWKN77-BCRWE67-56HX5IA-E4IDBCI-WE46DK3-EC63DQ7";
        folders.sync = { };
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
      ssh.hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN/PMBThi4DhgZR8VywbRDzzMVh2Qp3T6NJAcPubfXz6";
      syncthing = {
        id = "3L2KCXM-D75XCVU-5JLMV6V-FKQID2K-LJA6GFB-R2G77LD-5WXFHJT-BB4B7Q5";
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
        };
      };
    };
    rhea = {
      net = {
        ips = [ "192.168.1.50" ];
        vpn = {
          pubkey = "QBGdlPgtaLIh+WDLbuIWPL+Nr08mtfIqs6RwgVDAGjA=";
          ips = [ "10.100.0.50" ];
        };
        names = [
          "rhea.home"
          "rhea.vpn"
          "rhea.sbr.pm"
        ];
      };
      ssh.hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKFH3Lk4bRgNyFRK/Hzg1PvVbL/dpyI1SmLJFkb6VQDw";
      syncthing = {
        id = "YORNSGU-UC4IAG5-IWJCD7T-MVPIU7O-AYM36UK-LEHF7AP-CBC4L6C-ZWKUYQF";
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
          photos.path = "/neo/pictures/photos";
          music.path = "/neo/music";
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
      ssh.hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMs2o62unBFN/LHRg3q2N4QyZW0+DC/gjw3yzRbWdzx5";
      syncthing = {
        id = "YORNSGU-UC4IAG5-IWJCD7T-MVPIU7O-AYM36UK-LEHF7AP-CBC4L6C-ZWKUYQF";
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
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
      ssh.hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDWdnPJg0Y4kd4lHPAGE4xgMAK2qvMg3oBxh0t+xO+7O";
    };
    wakasu = {
      net = {
        vpn.ips = [ "10.100.0.8" ];
        names = [
          "wakasu.vpn"
          "wakasu.sbr.pm"
        ];
      };
      syncthing = {
        id = "WM23THJ-ECXRLXA-HE5TIKO-VPLSMRY-Y2EWZI7-Q7JMLPX-5Q5UNEN-QMB7ZQJ";
        folders = {
          org = { };
          documents = { };
          sync = { };
          screenshots = { };
          wallpapers = { };
        };
      };
    };
    okinawa = {
      net = {
        ips = [ "192.168.1.19" ];
        vpn.ips = [ "10.100.0.14" ];
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
    hass = {
      net = {
        ips = [ "192.168.1.181" ];
        vpn.ips = [ "10.100.0.81" ];
        names = [
          "hass.home"
          "hass.vpn"
          "hass.sbr.pm"
        ];
      };
    };
  };
}
