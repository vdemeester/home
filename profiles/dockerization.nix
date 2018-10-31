# Docker configuration
{ config, pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    python27Packages.docker_compose
    docker-machine
  ];
  virtualisation = {
    docker = {
      enable = true;
      package = pkgs.docker-edge;
      liveRestore = false;
      storageDriver = "overlay2";
      extraOptions = "--label=type=desktop --experimental --init --debug --add-runtime docker-runc=${pkgs.runc-edge}/bin/runc --default-runtime=docker-runc --containerd=/run/containerd/containerd.sock --insecure-registry 172.30.0.0/16";
    };
  };
  networking.firewall.trustedInterfaces = [ "docker0" ];
}
