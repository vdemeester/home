{ pkgs, ...}:

{
  imports = [ ./base.nix ];
  home.packages = with pkgs; [
    envsubst
    nur.repos.vdemeester.fhs-std
  ];
}
