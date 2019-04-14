{
  imports = [
    ./base.nix
  ];
  xdg.configFile."ape.conf".source = ../assets/ape.conf;
}
