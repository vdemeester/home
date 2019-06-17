{
  imports = [
    ./base.nix
  ];
  profiles.gpg.enable = true;
  xdg.configFile."ape.conf".source = ../assets/ape.conf;
}
