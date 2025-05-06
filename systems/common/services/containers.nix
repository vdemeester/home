_: {
  networking.firewall.checkReversePath = false;
  virtualisation.podman.enable = true;
  virtualisation.containers = {
    enable = true;
    registries = {
      search = [
        "registry.fedoraproject.org"
        "registry.access.redhat.com"
        "registry.centos.org"
        "docker.io"
        "quay.io"
      ];
    };
    policy = {
      default = [ { type = "insecureAcceptAnything"; } ];
      transports = {
        docker-daemon = {
          "" = [ { type = "insecureAcceptAnything"; } ];
        };
      };
    };
    containersConf.settings = {
      network = {
        default_subnet_pools = [
          # See https://github.com/kubernetes-sigs/kind/issues/2872 for this
          {
            "base" = "11.0.0.0/24";
            "size" = 24;
          }
          {
            "base" = "192.168.129.0/24";
            "size" = 24;
          }
          {
            "base" = "192.168.130.0/24";
            "size" = 24;
          }
          {
            "base" = "192.168.131.0/24";
            "size" = 24;
          }
          {
            "base" = "192.168.132.0/24";
            "size" = 24;
          }
        ];
      };
    };
  };
}
