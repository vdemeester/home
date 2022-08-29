{
  imports = [
    ./boot.nix
    ./config.nix
    ./nix.nix
    ./users.nix
    ./binfmt.nix
  ];

  boot = {
    cleanTmpDir = true;
  };
  # FIXME fix tmpOnTmpfs
  systemd.additionalUpstreamSystemUnits = [ "tmp.mount" ];
}
