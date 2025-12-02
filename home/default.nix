{
  config,
  desktop,
  hostname,
  lib,
  stateVersion,
  username,
  inputs,
  libx,
  ...
}:
{
  imports = [
    inputs.niri.homeModules.niri
    ./common/shell
  ]
  ++ lib.optional (builtins.isString desktop) ./common/desktop
  ++ lib.optional (builtins.pathExists (./. + "/common/users/${username}")) ./common/users/${username}
  ++ lib.optional (
    config ? osConfig
    && builtins.hasAttr "${hostname}" config.osConfig.infrastructure.machines
    && libx.hasSyncthingFolders config.osConfig.infrastructure.machines."${hostname}"
  ) ./common/services/syncthing.nix
  ++ lib.optional (builtins.pathExists (
    ../systems/. + "/${hostname}/home.nix"
  )) ../systems/${hostname}/home.nix;

  home = {
    inherit username stateVersion;
    homeDirectory = "/home/${username}";
  };

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    use-xdg-base-directories = true;
  };
}
