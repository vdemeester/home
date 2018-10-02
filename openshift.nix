{ pkgs, ... }:

{
  xdg.configFile."fish/conf.d/openshift.fish".source = ./fish/openshift.fish;
  home.packages = with pkgs; [
    docker-machine-kvm
    docker-machine-kvm2
    s2i
  ];
}
