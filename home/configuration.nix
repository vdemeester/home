{ pkgs, prefix, ... }:

{
  home.packages = with pkgs; [
    acpi
    autorandr
    jq
    htop
    pass
  ];
}
