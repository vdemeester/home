{
  imports = [
    ./core
    ./hardware
    ./profiles
    ./programs
    ./services
    ./virtualisation
  ];
  sops.defaultSopsFile = ../../secrets/secrets.yaml;
}
