{ config, pkgs, ... }:

{
  imports = [ ./containerd.nix ];
  environment.systemPackages = with pkgs; [
    buildkit
  ];
  virtualisation = {
    buildkitd= {
      enable = true;
      package = pkgs.buildkit;
      packages = [ pkgs.runc-edge pkgs.git ];
      extraOptions = "--oci-worker=false --containerd-worker=true";
    };
  };
}
