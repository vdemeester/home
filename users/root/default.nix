{ config, lib, pkgs, ... }:

with lib; {
  users.users.root = {
    shell = mkIf config.programs.zsh.enable pkgs.zsh;
  };
  home-manager.users.root = lib.mkMerge (
    [
      (import ../vincent/core/zsh.nix)
      (import ./home.nix)
    ]
    ++ optionals (versionOlder config.system.nixos.release "21.11") [{
      # FIXME manpages are broken on 21.05 and home-manager (for some reason..)
      manual.manpages.enable = false;
    }]
  );
}
