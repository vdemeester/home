{
  imports = [
    ./config.nix
    ./home-manager.nix
    ./nix.nix
    ./nur.nix
    ./users.nix
  ];

  boot = {
    cleanTmpDir = true;
  };
}
