{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.jellyfin-auto-collections;

  # Convert schedule shortcuts to systemd OnCalendar format
  scheduleToCalendar =
    schedule:
    if schedule == "hourly" then
      "hourly"
    else if schedule == "daily" then
      "daily"
    else if schedule == "weekly" then
      "weekly"
    else if schedule == "monthly" then
      "monthly"
    else
      schedule;

  # Generate complete YAML config
  # We'll use a template and substitute secrets at runtime
  configYaml = lib.generators.toYAML { } (
    {
      jellyfin = {
        server_url = cfg.jellyfinUrl;
        api_key = "JELLYFIN_API_KEY_PLACEHOLDER";
        user_id = cfg.userId;
      };
    }
    // lib.optionalAttrs (cfg.jellyseerr.enable && cfg.jellyseerr.passwordFile != null) {
      jellyseerr = {
        server_url = cfg.jellyseerr.serverUrl;
        email = cfg.jellyseerr.email;
        password = "JELLYSEERR_PASSWORD_PLACEHOLDER";
        user_type = cfg.jellyseerr.userType;
      };
    }
    // cfg.settings
  );

  # Script to generate config file and run the application
  startScript = pkgs.writeShellScript "jellyfin-auto-collections-start" ''
        # Load API key from file if specified
        ${lib.optionalString (cfg.apiKeyFile != null) ''
          JELLYFIN_API_KEY=$(cat ${cfg.apiKeyFile})
        ''}

        # Load Jellyseerr password from file if specified
        ${lib.optionalString (cfg.jellyseerr.enable && cfg.jellyseerr.passwordFile != null) ''
          JELLYSEERR_PASSWORD=$(cat ${cfg.jellyseerr.passwordFile})
        ''}

        # Generate config.yml by substituting the placeholders
        cat > ${cfg.dataDir}/config.yml << 'EOF'
    ${configYaml}
    EOF

        # Replace the placeholders with actual secrets
        sed -i "s/JELLYFIN_API_KEY_PLACEHOLDER/$JELLYFIN_API_KEY/g" ${cfg.dataDir}/config.yml
        ${lib.optionalString (cfg.jellyseerr.enable && cfg.jellyseerr.passwordFile != null) ''
          sed -i "s/JELLYSEERR_PASSWORD_PLACEHOLDER/$JELLYSEERR_PASSWORD/g" ${cfg.dataDir}/config.yml
        ''}

        chmod 600 ${cfg.dataDir}/config.yml

        # Run the main script with config file
        exec ${cfg.package}/bin/jellyfin-auto-collections --config ${cfg.dataDir}/config.yml
  '';
in
{
  options.services.jellyfin-auto-collections = {
    enable = mkEnableOption "Jellyfin Auto Collections service";

    package = mkOption {
      type = types.package;
      default = pkgs.jellyfin-auto-collections;
      defaultText = literalExpression "pkgs.jellyfin-auto-collections";
      description = "The jellyfin-auto-collections package to use.";
    };

    schedule = mkOption {
      type = types.str;
      default = "daily";
      description = ''
        When to run the collection update. Can be "hourly", "daily", "weekly", "monthly", or a systemd OnCalendar format.
        See systemd.time(7) for OnCalendar format details.
      '';
      example = "daily";
    };

    jellyfinUrl = mkOption {
      type = types.str;
      example = "http://localhost:8096";
      description = "URL of the Jellyfin server";
    };

    apiKeyFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        Path to a file containing the Jellyfin API key.
        The file should contain only the API key.
      '';
    };

    userId = mkOption {
      type = types.str;
      description = "Jellyfin user ID";
    };

    jellyseerr = {
      enable = mkEnableOption "Jellyseerr integration";

      serverUrl = mkOption {
        type = types.str;
        default = "http://localhost:5055";
        description = "URL of the Jellyseerr server";
      };

      email = mkOption {
        type = types.str;
        description = "Jellyseerr user email";
      };

      passwordFile = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = ''
          Path to a file containing the Jellyseerr password.
          The file should contain only the password.
        '';
      };

      userType = mkOption {
        type = types.str;
        default = "local";
        description = "Jellyseerr user type (local or plex)";
      };
    };

    settings = mkOption {
      type = types.attrs;
      default = { };
      description = ''
        Configuration for Jellyfin Auto Collections.
        This will be converted to a config file.
      '';
      example = literalExpression ''
        {
          jellyfin = {
            url = "http://localhost:8096";
            user_id = "your-user-id";
          };
          lists = [
            {
              type = "imdb";
              url = "https://www.imdb.com/list/ls123456789/";
              name = "IMDb Top 250";
            }
          ];
        }
      '';
    };

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/jellyfin-auto-collections";
      description = "Directory to store jellyfin-auto-collections data and cache";
    };

    user = mkOption {
      type = types.str;
      default = "jellyfin-auto-collections";
      description = "User account under which jellyfin-auto-collections runs.";
    };

    group = mkOption {
      type = types.str;
      default = "jellyfin-auto-collections";
      description = "Group under which jellyfin-auto-collections runs.";
    };
  };

  config = mkIf cfg.enable {
    # Create the user and group
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.group;
      home = cfg.dataDir;
      createHome = true;
    };

    users.groups.${cfg.group} = { };

    # Systemd service
    systemd.services.jellyfin-auto-collections = {
      description = "Jellyfin Auto Collections";
      after = [ "network.target" ];

      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        Group = cfg.group;
        WorkingDirectory = cfg.dataDir;

        ExecStart = "${startScript}";

        # Security hardening
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [ cfg.dataDir ];
        NoNewPrivileges = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        PrivateMounts = true;
        LockPersonality = true;
      };

      environment = {
        JELLYFIN_SERVER_URL = cfg.jellyfinUrl;
        JELLYFIN_USER_ID = cfg.userId;
        HOME = cfg.dataDir;
      };
    };

    # Systemd timer
    systemd.timers.jellyfin-auto-collections = {
      description = "Timer for Jellyfin Auto Collections";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = scheduleToCalendar cfg.schedule;
        Persistent = true;
        RandomizedDelaySec = "5m";
      };
    };
  };
}
