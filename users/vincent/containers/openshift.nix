{ pkgs, ... }:

{
  home.file.".local/share/applications/chos4.desktop".source = ./chos4.desktop;
  home.packages = with pkgs; [
    #my.crc
    oc
    openshift-install
    operator-sdk
    opm
    my.operator-tool
  ];
}
