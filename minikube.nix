{ pkgs, prefix, ... }:

{
  home.packages = with pkgs; [
    minikube
  ];
}
