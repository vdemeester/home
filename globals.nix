{ lib, ... }:
{
  net = {
    vpn = {
      endpoint = "167.99.17.238";
      pubkey = "+H3fxErP9HoFUrPgU19ra9+GDLQw+VwvLWx3lMct7QI=";
    };
  };
  machines = {
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
    wg-ips = ips: builtins.map (x: "${x}/24") ips;
    hasSyncthingFolders = host: (builtins.length (lib.attrsets.attrValues host.syncthing.folders)) > 0;
  };
}
