{
  config,
  desktop,
  hostname,
  lib,
  outputs,
  stateVersion,
  username,
  inputs,
  globals,
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
    builtins.hasAttr "${hostname}" globals.machines
    && libx.hasSyncthingFolders globals.machines."${hostname}"
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

  # nixpkgs = {
  #   overlays = [
  #     # Our own flake exports (from overlays and pkgs dir)
  #     outputs.overlays.additions
  #     outputs.overlays.modifications
  #     outputs.overlays.unstable-packages
  #
  #     # And from other flakes
  #     inputs.emacs-overlay.overlay
  #     inputs.chapeau-rouge.overlays.openshift
  #     inputs.chick-group.overlays.default
  #     inputs.agenix.overlays.default
  #     inputs.niri.overlays.niri
  #
  #     # Migrate to "modifications"
  #     (_: prev: {
  #       inherit (inputs.buildkit-tekton.packages.${prev.system}) tkn-local;
  #       inherit (inputs.dagger.packages.${prev.system}) dagger;
  #     })
  #   ];
  #   config = {
  #     allowUnfree = true;
  #     # Workaround for https://github.com/nix-community/home-manager/issues/2942
  #     allowUnfreePredicate = _: true;
  #   };
  # };
}
