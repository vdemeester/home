{
  imports = [
    ./base.nixos.nix
  ];
  xdg.configFile."ape.conf".source = ../assets/ape.conf;
}
