{
  imports = [
    ./base.nixos.nix
  ];
  profiles.git.enable = true;
  xdg.configFile."ape.conf".source = ../assets/ape.conf;
}
