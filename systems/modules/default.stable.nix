{
  imports = [
    ./core
    ./hardware
    ./profiles/default.stable.nix
    ./programs
    ./services
    ./virtualisation
    "${(import ../../nix/sources.nix).sops-nix}/modules/sops"
  ];
  sops.defaultSopsFile = ../../secrets/secrets.yaml;
}
