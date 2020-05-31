{ config, lib, pkgs, ... }:

{
  home-manager.users.root = lib.mkMerge (
    [ (import ../vincent/core) ]
  );
}
