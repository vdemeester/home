{ lib, desktop, ... }:
{
  imports = [
    ./ai.nix
    ./go.nix
    ./lua.nix
    ./nix.nix
    ./python.nix
    ./base.nix
  ]
  ++ lib.optional (builtins.isString desktop) ./desktop.nix;
}
