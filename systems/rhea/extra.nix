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
    aria2 = {
      enable = true;
      openPorts = true;
      settings = {
        max-concurrent-downloads = 20;
      };
      downloadDir = "/link/downloads";
      rpcSecretFile = "${pkgs.writeText "aria" "aria2rpc\n"}"; # FIXME: use secrets for this somehow
    };
    transmission = {
      enable = true;
    };
    sonarr = {
      enable = true;
      # user = "vincent";
      # group = "users";
    };
    radarr = {
      enable = true;
    };
    bazarr = {
      enable = true;
    };
    prowlarr = {
      enable = true;
    };
    readarr = {
      enable = true;
    };
    lidarr = {
      enable = true;
    };
  };

  networking.useDHCP = lib.mkDefault true;

  environment.systemPackages = with pkgs; [
    lm_sensors
    gnumake
  ];

}
