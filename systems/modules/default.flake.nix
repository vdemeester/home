{ ... }:

{
  imports = [
    ./desktop
    ./editors
    ./hardware
    ./profiles/default.flake.nix
    ./programs
    ./services
    ./shell
    ./virtualisation/default.flake.nix
  ];
}
