{ pkgs, prefix, ... }:

{
  imports = [ ./dev.nix ];
  home.packages = with pkgs; [
    python3
    pipenv
  ];
}
