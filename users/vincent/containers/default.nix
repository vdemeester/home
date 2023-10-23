{ pkgs, ... }:

{
  imports = [
    ./gcloud.nix
  ];

  home.packages = with pkgs; [
    skopeo
    my.manifest-tool
    nerdctl
    act
    # dagger – Remove due to trademark issues : https://github.com/NixOS/nixpkgs/issues/260848, will have to package myself
  ];
}
