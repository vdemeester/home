{ config, pkgs, ... }:

{
  imports = [ ./containerd.nix ../service/buildkit.nix ];
  environment.systemPackages = with pkgs; [
    buildkit
  ];
  virtualisation = {
    buildkitd= {
      enable = true;
      package = pkgs.buildkit;
      extraOptions = "--oci-worker=false --containerd-worker=true";
    };
  };
}
