{ config, lib, pkgs, ... }:

let
  inherit (lib) lists attrsets mkIf optionals versionOlder;
  secretPath = ../../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);

  isAuthorized = p: builtins.isAttrs p && p.authorized or false;
  authorizedKeys = lists.optionals secretCondition (
    attrsets.mapAttrsToList
      (name: value: value.key)
      (attrsets.filterAttrs (name: value: isAuthorized value) (import secretPath).ssh)
  );
in
{
  users.users.root = {
    shell = mkIf config.programs.zsh.enable pkgs.zsh;
    openssh.authorizedKeys.keys = authorizedKeys;
  };
  home-manager.users.root = lib.mkMerge (
    [
      (import ../vincent/core/zsh.nix)
      (import ../vincent/core/ssh.nix)
      (import ./home.nix)
    ]
    ++ optionals (versionOlder config.system.nixos.release "21.11") [{
      # FIXME manpages are broken on 21.05 and home-manager (for some reason..)
      manual.manpages.enable = false;
    }]
  );
}
