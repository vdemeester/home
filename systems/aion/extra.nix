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
      ips = libx.wg-ips globals.machines.aion.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };

    rsync-replica = {
      enable = true;
      jobs = {
        rhea-backup = {
          source = {
            host = "rhea.sbr.pm";
            user = "vincent";
            paths = [
              "/neo/documents"
              "/neo/music"
              "/neo/pictures"
              "/neo/videos"
            ];
          };
          destination = "/neo";
          schedule = "daily";
          delete = true; # Mirror mode: delete files in destination that don't exist in source
          user = "vincent";
          group = "users";
          rsyncArgs = [
            "--exclude=.Trash-*"
            "--exclude=lost+found"
          ];
          sshArgs = [
            "-o StrictHostKeyChecking=accept-new"
          ];
        };
      };
    };
  };

  networking.useDHCP = lib.mkDefault true;

  environment.systemPackages = with pkgs; [
    lm_sensors
    gnumake
  ];

}
