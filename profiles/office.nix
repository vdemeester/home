{ configs, pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    calibre
    libreoffice
    gimp
  ];
}
