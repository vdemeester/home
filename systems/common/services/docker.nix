{ pkgs, ... }:
{
  system.nixos.tags = [ "docker" ];
  virtualisation = {
    docker = {
      enable = true;
      liveRestore = false;
      storageDriver = "overlay2";
      daemon.settings = {
        userland-proxy = true;
        experimental = true;
        bip = "172.26.0.1/16";
        metrics-addr = "0.0.0.0:9323";
        features = {
          buildkit = true;
        };
        insecure-registries = [
          "172.30.0.0/16"
          "192.168.1.0/16"
          "10.100.0.0/16"
          "shikoku.home:5000"
          "r.svc.home:5000"
          "r.svc.home"
        ];
        # seccomp-profile = ./my-seccomp.json;
      };
    };
  };
  environment.systemPackages = with pkgs; [ docker-buildx ];
  networking.firewall.trustedInterfaces = [ "docker0" ];
  networking.firewall.checkReversePath = false;
  networking.firewall.allowedTCPPorts = [ 9323 ]; # Docker Prometheus metrics
}
