{ pkgs, ... }:

{
  home.packages = with pkgs; [
    docker-machine-kvm
    docker-machine-kvm2
    s2i
    openshift
  ];
}
