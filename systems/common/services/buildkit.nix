_: {
  virtualisation = {
    containerd = {
      enable = true;
    };
    buildkitd = {
      enable = true;
      settings = {
        grpc = {
          # This can be overridden per host
          address = [ "unix:///run/buildkit/buildkitd.sock" ];
          ;
          };
          worker.oci = {
            enabled = false;
          };
          worker.containerd = {
            enabled = true;
            platforms = [ "linux/amd64" "linux/arm64" ];
            namespace = "buildkit";
          };
          # FIXME: change thoses
          registry = {
            "r.svc.home:5000" = {
              http = true;
              insecure = true;
            };
            "r.svc.home" = {
              http = true;
              insecure = true;
            };
          };
        };
      };
    };
  }
