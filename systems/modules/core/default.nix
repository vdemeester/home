{
  imports = [
    ./config.nix
    ./nix.nix
    ./users.nix
  ];

  boot = {
    cleanTmpDir = true;
  };
}
