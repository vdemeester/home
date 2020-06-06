{
  imports = [
    (import ../../../nix).home-manager
    ./home-manager.nix
    ./nix.nix
    ./nur.nix
  ];

  boot = {
    cleanTmpDir = true;
  };
}
