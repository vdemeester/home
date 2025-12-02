{
  libx,
  lib,
  pkgs,
  config,
  ...
}:
{
  users.users.vincent.linger = true;

  services = {
    wireguard = {
      enable = true;
      ips = libx.wg-ips config.infrastructure.machine.network.vpn.ips;
      endpoint = config.infrastructure.vpn.endpoint;
      endpointPublicKey = "+H3fxErP9HoFUrPgU19ra9+GDLQw+VwvLWx3lMct7QI="; # kerkouane
    };

    immich = {
      enable = true;
      user = "vincent";
      group = "users";
      mediaLocation = "/neo/pictures/photos";
    };

    postgresql = {
      ensureDatabases = [ "immich" ];
      ensureUsers = [
        {
          name = "vincent";
        }
      ];
    };
  };

  # Grant vincent ownership and superuser privileges for the immich database
  systemd.services.postgresql.postStart = lib.mkAfter ''
    PSQL="${config.services.postgresql.package}/bin/psql --port=${toString config.services.postgresql.settings.port}"
    $PSQL -tAc "SELECT 1 FROM pg_roles WHERE rolname = 'vincent'" | grep -q 1 || $PSQL -tAc "CREATE ROLE vincent WITH LOGIN SUPERUSER"
    $PSQL -tAc "ALTER ROLE vincent WITH SUPERUSER"
    $PSQL -tAc "ALTER DATABASE immich OWNER TO vincent"
    $PSQL immich -tAc "ALTER SCHEMA public OWNER TO vincent"
    $PSQL immich -tAc "GRANT ALL PRIVILEGES ON SCHEMA public TO vincent"
    $PSQL immich -tAc "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO vincent"
    $PSQL immich -tAc "GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO vincent"
    $PSQL immich -tAc "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO vincent"
  '';

  networking.useDHCP = lib.mkDefault true;

  environment.systemPackages = with pkgs; [
    lm_sensors
    gnumake
  ];

}
