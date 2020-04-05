let
  hostName = "${builtins.readFile ./hostname}";
in
rec {
  imports = [
    # Default profile with default configuration
    ./modules/module-list.nix
    # Machine specific configuration files
    (./machines + "/${hostName}.nix")
  ];
}
