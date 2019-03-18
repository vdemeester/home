{
  imports = [
    ./base.nix
  ];
  profiles.media.enable = true;
  xdg.configFile."ape.conf".source = ../assets/ape.conf;
}
