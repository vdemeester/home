{
  libx,
  globals,
  config,
  ...
}:
{
  imports = [
    ../common/services/bind.nix
    ../common/services/prometheus-exporters-node.nix
    ../common/services/prometheus-exporters-bind.nix
  ];

  networking.firewall.enable = false;

  # TODO make it an option ? (otherwise I'll add it for all)
  users.users.vincent.linger = true;
  services = {
    mosquitto = {
      enable = true;
      listeners = [
        {
          address = "0.0.0.0";
          port = 1883;
          omitPasswordAuth = false;
          settings = {
            allow_anonymous = false;
          };
          acl = [ "topic readwrite #" ];
          users = {
            homeassistant = {
              acl = [ "readwrite #" ];
              hashedPasswordFile = config.age.secrets."mosquitto-homeassistant-password".path;
            };
          };
        }
      ];
    };

    prometheus.exporters.mqtt = {
      enable = true;
      port = 9234;
      mqttAddress = "127.0.0.1";
      mqttPort = 1883;
      mqttTopic = "#"; # Subscribe to all topics
      mqttUsername = "homeassistant";
      environmentFile = config.age.secrets."mosquitto-homeassistant-password".path;
      logLevel = "INFO";
    };

    wireguard = {
      enable = true;
      ips = libx.wg-ips globals.machines.demeter.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
  };

  age.secrets."mosquitto-homeassistant-password" = {
    file = ../../secrets/demeter/mosquitto-homeassistant-password.age;
    mode = "400";
    owner = "mosquitto";
    group = "mosquitto";
  };
}
