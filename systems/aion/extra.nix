{
  libx,
  globals,
  lib,
  pkgs,
  ...
}:
let
  # Common rsync configuration for rhea backups
  rheaBackupDefaults = {
    source = {
      host = "rhea.sbr.pm";
      user = "vincent";
    };
    destination = "/neo";
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
in
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
        rhea-daily = rheaBackupDefaults // {
          source = rheaBackupDefaults.source // {
            paths = [
              "/neo/audiobooks"
              "/neo/documents"
              "/neo/ebooks"
            ];
          };
          schedule = "daily";
        };
        rhea-hourly = rheaBackupDefaults // {
          source = rheaBackupDefaults.source // {
            paths = [
              "/neo/music"
              "/neo/pictures"
              "/neo/videos"
            ];
          };
          schedule = "hourly";
        };
      };
    };

    navidrome = {
      enable = true;
      settings = {
        MusicFolder = "/neo/music";
        Address = "0.0.0.0";
        Port = 4533;
        BaseURL = "https://music.sbr.pm";

        # Paths
        DataFolder = "/var/lib/navidrome";
        CacheFolder = "/var/cache/navidrome";

        # Features
        EnableTranscodingConfig = true;
        EnableSubsonic = true;

        # Optional: Scrobbling (can enable later)
        # LastFM.Enabled = true;
      };
    };
  };

  networking = {
    useDHCP = lib.mkDefault true;
    firewall.allowedTCPPorts = [ 4533 ]; # Navidrome
  };

  environment.systemPackages = with pkgs; [
    lm_sensors
    gnumake
  ];

}
