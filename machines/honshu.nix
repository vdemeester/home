{
  imports = [
    ./base.nix
  ];
  profiles.emacs = { daemonService = false; };
  profiles.dev = {
    go.enable = true;
  };
  profiles.media.enable = true;
  xdg.configFile."ape.conf".source = ../assets/ape.conf;
}
