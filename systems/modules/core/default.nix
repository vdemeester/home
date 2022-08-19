{
  imports = [
    #./boot.nix
    ./config.nix
    ./nix.nix
    ./users.nix
  ];

  boot = {
    cleanTmpDir = true;
  };
  # FIXME fix tmpOnTmpfs
  systemd.additionalUpstreamSystemUnits = [ "tmp.mount" ];
}
