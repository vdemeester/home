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

    # Dual-hub client: connect to both local hub (self) and remote hub (kerkouane)
    wireguard.dualClient = {
      enable = true;
      ips = libx.wg-ips globals.machines.demeter.net.vpn.ips;

      # Local hub (self)
      localHub = {
        enable = true;
        endpoint = builtins.head globals.machines.demeter.net.ips;
        endpointPort = globals.net.localHub.port;
        endpointPublicKey = globals.machines.demeter.net.vpn.pubkey;
      };

      # Remote hub (kerkouane)
      remoteHub = {
        endpoint = globals.net.vpn.endpoint;
        endpointPort = globals.net.vpn.port;
        endpointPublicKey = globals.machines.kerkouane.net.vpn.pubkey;
      };
    };

    # Local hub server for home network
    wireguard.localHub = {
      enable = true;
      ips = libx.wg-ips globals.machines.demeter.net.vpn.ips;
      listenAddress = builtins.head globals.machines.demeter.net.ips;
      listenPort = globals.net.localHub.port;
      peers = libx.generateWireguardPeers globals.machines;
    };
  };

  age.secrets."mosquitto-homeassistant-password" = {
    file = ../../secrets/demeter/mosquitto-homeassistant-password.age;
    mode = "400";
    owner = "mosquitto";
    group = "mosquitto";
  };
}
