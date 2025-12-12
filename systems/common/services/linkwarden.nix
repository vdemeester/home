{ pkgs, ... }:
{
  # Linkwarden - Self-hosted collaborative bookmark manager
  # https://linkwarden.app/
  #
  # Replacement for Omnivore (which shut down in November 2024)
  # Features: Full-page preservation, reader view, annotations, AI tagging

  services.linkwarden = {
    enable = true;

    # Network configuration
    host = "0.0.0.0";
    port = 3002;

    # Storage
    storageLocation = "/var/lib/linkwarden";
    cacheLocation = "/var/cache/linkwarden";

    # Database (auto-configured PostgreSQL)
    database = {
      createLocally = true;
      name = "linkwarden";
      user = "linkwarden";
    };

    # Allow user registration
    enableRegistration = true;

    # Secret files
    # TODO: Move to agenix for production
    secretFiles.NEXTAUTH_SECRET = "${pkgs.writeText "nextauth-secret" ''
      changeme-replace-with-agenix-secret-in-production
    ''}";

    # Environment variables
    environment = {
      PAGINATION_TAKE_COUNT = "24";
      AUTOSCROLL_TIMEOUT = "30";
      RE_ARCHIVE_LIMIT = "5";
      # STORAGE_FOLDER is set automatically by the module
      # Disable telemetry for privacy
      NEXT_PUBLIC_DISABLE_REGISTRATION = "false";
    };
  };

  # Ensure PostgreSQL is configured
  services.postgresql = {
    ensureDatabases = [ "linkwarden" ];
    ensureUsers = [
      {
        name = "linkwarden";
        ensureDBOwnership = true;
      }
    ];
  };

  # Open firewall for local access (Traefik will proxy)
  networking.firewall.allowedTCPPorts = [ 3002 ];
}
