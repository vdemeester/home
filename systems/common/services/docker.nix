_: {
  virtualisation = {
    docker = {
      enable = true;
      liveRestore = false;
      storageDriver = "overlay2";
      daemon.settings = {
        userland-proxy = false;
        experimental = true;
        bip = "172.26.0.1/16";
        features = { buildkit = true; };
        insecure-registries = [ "172.30.0.0/16" "192.168.1.0/16" "10.100.0.0/16" "shikoku.home:5000" "r.svc.home:5000" "r.svc.home" ];
        # seccomp-profile = ./my-seccomp.json;
      };
    };
  };
  environment.systemPackages = with pkgs; [ docker-buildx ];
  networking.firewall.trustedInterfaces = [ "docker0" ];
}
