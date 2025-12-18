# Prometheus alert rules for homelab monitoring
# Based on: ~/desktop/org/notes/*prometheus*.org
{
  groups = [
    {
      name = "node_alerts";
      interval = "30s";
      rules = [
        # Disk space warnings
        {
          alert = "DiskSpaceLow";
          expr = ''
            (node_filesystem_avail_bytes{fstype!="tmpfs",fstype!="ramfs",fstype!="squashfs"} / node_filesystem_size_bytes{fstype!="tmpfs",fstype!="ramfs",fstype!="squashfs"}) * 100 < 15
          '';
          for = "5m";
          labels = {
            severity = "warning";
          };
          annotations = {
            summary = "Disk space low on {{ $labels.instance }}";
            description = "Filesystem {{ $labels.mountpoint }} on {{ $labels.instance }} has less than 15% space remaining ({{ $value | humanizePercentage }})";
          };
        }
        {
          alert = "DiskSpaceCritical";
          expr = ''
            (node_filesystem_avail_bytes{fstype!="tmpfs",fstype!="ramfs",fstype!="squashfs"} / node_filesystem_size_bytes{fstype!="tmpfs",fstype!="ramfs",fstype!="squashfs"}) * 100 < 5
          '';
          for = "2m";
          labels = {
            severity = "critical";
          };
          annotations = {
            summary = "Disk space critical on {{ $labels.instance }}";
            description = "Filesystem {{ $labels.mountpoint }} on {{ $labels.instance }} has less than 5% space remaining ({{ $value | humanizePercentage }})";
          };
        }

        # High CPU usage
        {
          alert = "HighCPUUsage";
          expr = ''
            100 - (avg by (instance) (rate(node_cpu_seconds_total{mode="idle"}[5m])) * 100) > 80
          '';
          for = "10m";
          labels = {
            severity = "warning";
          };
          annotations = {
            summary = "High CPU usage on {{ $labels.instance }}";
            description = "CPU usage on {{ $labels.instance }} is above 80% (current: {{ $value | humanizePercentage }})";
          };
        }

        # High memory usage
        {
          alert = "HighMemoryUsage";
          expr = ''
            (1 - (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)) * 100 > 90
          '';
          for = "5m";
          labels = {
            severity = "warning";
          };
          annotations = {
            summary = "High memory usage on {{ $labels.instance }}";
            description = "Memory usage on {{ $labels.instance }} is above 90% (current: {{ $value | humanizePercentage }})";
          };
        }

        # Node down
        {
          alert = "NodeDown";
          expr = "up{job=\"node\"} == 0";
          for = "2m";
          labels = {
            severity = "critical";
          };
          annotations = {
            summary = "Node exporter down on {{ $labels.instance }}";
            description = "Node exporter on {{ $labels.instance }} has been down for more than 2 minutes";
          };
        }
      ];
    }

    {
      name = "service_alerts";
      interval = "30s";
      rules = [
        # Service exporters down
        {
          alert = "ServiceExporterDown";
          expr = "up{job!=\"node\"} == 0";
          for = "5m";
          labels = {
            severity = "warning";
          };
          annotations = {
            summary = "Service exporter down: {{ $labels.job }}";
            description = "Service exporter {{ $labels.job }} on {{ $labels.instance }} has been down for more than 5 minutes";
          };
        }

        # PostgreSQL down
        {
          alert = "PostgreSQLDown";
          expr = "pg_up == 0";
          for = "2m";
          labels = {
            severity = "critical";
          };
          annotations = {
            summary = "PostgreSQL down on {{ $labels.instance }}";
            description = "PostgreSQL database on {{ $labels.instance }} has been unreachable for more than 2 minutes";
          };
        }

        # Traefik down
        {
          alert = "TraefikDown";
          expr = "up{job=\"traefik\"} == 0";
          for = "2m";
          labels = {
            severity = "critical";
          };
          annotations = {
            summary = "Traefik reverse proxy down";
            description = "Traefik on rhea.sbr.pm has been down for more than 2 minutes - all web services may be inaccessible";
          };
        }
      ];
    }

    {
      name = "dns_alerts";
      interval = "30s";
      rules = [
        # BIND DNS service down
        {
          alert = "DNSServiceDown";
          expr = "up{job=\"bind\"} == 0";
          for = "2m";
          labels = {
            severity = "critical";
          };
          annotations = {
            summary = "DNS service down on {{ $labels.instance }}";
            description = "BIND DNS service on {{ $labels.instance }} has been unreachable for more than 2 minutes - DNS resolution may fail";
          };
        }

        # High DNS query failure rate
        {
          alert = "HighDNSQueryFailureRate";
          expr = "rate(bind_query_errors_total[5m]) > 10";
          for = "5m";
          labels = {
            severity = "warning";
          };
          annotations = {
            summary = "High DNS query failure rate on {{ $labels.instance }}";
            description = "DNS query failure rate on {{ $labels.instance }} is above 10 queries/sec";
          };
        }
      ];
    }

    {
      name = "traefik_alerts";
      interval = "30s";
      rules = [
        # Traefik certificate expiration warning
        {
          alert = "TraefikCertificateExpiringSoon";
          expr = "(traefik_tls_certs_not_after - time()) / 86400 < 7";
          for = "1h";
          labels = {
            severity = "warning";
          };
          annotations = {
            summary = "Traefik TLS certificate expiring soon";
            description = "TLS certificate for {{ $labels.cn }} will expire in less than 7 days";
          };
        }

        # High error rate (5xx responses)
        {
          alert = "TraefikHighErrorRate";
          expr = "rate(traefik_service_requests_total{code=~\"5..\"}[5m]) > 5";
          for = "5m";
          labels = {
            severity = "warning";
          };
          annotations = {
            summary = "High 5xx error rate on Traefik";
            description = "Service {{ $labels.service }} is returning 5xx errors at {{ $value }} req/sec";
          };
        }
      ];
    }

    {
      name = "caddy_alerts";
      interval = "30s";
      rules = [
        # Caddy down
        {
          alert = "CaddyDown";
          expr = "up{job=\"caddy\"} == 0";
          for = "2m";
          labels = {
            severity = "critical";
          };
          annotations = {
            summary = "Caddy web server down on kerkouane";
            description = "Caddy reverse proxy has been down for more than 2 minutes - external access may be broken";
          };
        }
      ];
    }

    {
      name = "media_services_alerts";
      interval = "1m";
      rules = [
        # Exportarr services down (sonarr, radarr, lidarr, prowlarr, bazarr)
        {
          alert = "MediaServiceDown";
          expr = "up{job=\"exportarr\"} == 0";
          for = "5m";
          labels = {
            severity = "warning";
          };
          annotations = {
            summary = "Media service down on {{ $labels.instance }}";
            description = "Exportarr exporter for {{ $labels.instance }} has been unreachable for 5 minutes - check *arr services";
          };
        }

        # Sonarr/Radarr queue backing up
        {
          alert = "MediaQueueBackingUp";
          expr = "sonarr_queue_total > 50 or radarr_queue_total > 50";
          for = "30m";
          labels = {
            severity = "warning";
          };
          annotations = {
            summary = "Media download queue backing up";
            description = "Download queue has {{ $value }} items - may indicate stuck downloads";
          };
        }
      ];
    }

    # MQTT alerts disabled - exporter package broken in nixpkgs
    # {
    #   name = "mqtt_alerts";
    #   interval = "30s";
    #   rules = [
    #     # Mosquitto MQTT broker down
    #     {
    #       alert = "MQTTBrokerDown";
    #       expr = "up{job=\"mosquitto\"} == 0";
    #       for = "2m";
    #       labels = {
    #         severity = "critical";
    #       };
    #       annotations = {
    #         summary = "MQTT broker down on demeter";
    #         description = "Mosquitto MQTT broker has been unreachable for more than 2 minutes - home automation may be affected";
    #       };
    #     }

    #     # MQTT high connection rate (potential issue)
    #     {
    #       alert = "MQTTHighConnectionRate";
    #       expr = "rate(mosquitto_connect_received[5m]) > 10";
    #       for = "5m";
    #       labels = {
    #         severity = "warning";
    #       };
    #       annotations = {
    #         summary = "High MQTT connection rate";
    #         description = "MQTT broker is seeing {{ $value }} connections/sec - may indicate reconnection loops";
    #       };
    #     }
    #   ];
    # }

    {
      name = "homeassistant_alerts";
      interval = "1m";
      rules = [
        # Home Assistant unreachable
        {
          alert = "HomeAssistantDown";
          expr = "up{job=\"homeassistant\"} == 0";
          for = "5m";
          labels = {
            severity = "warning";
          };
          annotations = {
            summary = "Home Assistant unreachable";
            description = "Home Assistant on hass.home has been unreachable for more than 5 minutes";
          };
        }
      ];
    }

    {
      name = "backup_alerts";
      interval = "1h";
      rules = [
        # Backup failures (we'll add these when we implement backup monitoring)
        # Placeholder for tarsnap, restic, rsync backup monitoring
      ];
    }
  ];
}
