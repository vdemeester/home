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
  ...
}:
{
  imports =
    [
      ./common/shell
    ]
    ++ lib.optional (builtins.isString desktop) ./common/desktop
    ++ lib.optional (builtins.pathExists (./. + "/common/users/${username}")) ./common/users/${username}
    ++ lib.optional (builtins.pathExists (
      ../systems/. + "/${hostname}/home.nix"
    )) ../systems/${hostname}/home.nix
    ++ lib.optional (globals.fn.hasSyncthingFolders
      globals.machines."${hostname}"
    ) ./common/services/syncthing.nix;

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

  nixpkgs = {
    overlays = [

      # FIXME remove those
      # (import ./nix/overlays).tekton
      (import ../nix/overlays/sbr.nix)
      # Our own flake exports (from overlays and pkgs dir)
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages

      # And from other flakes
      inputs.emacs-overlay.overlay
      inputs.chapeau-rouge.overlays.openshift
      inputs.chick-group.overlays.default
      inputs.agenix.overlays.default

      # Migrate to "modifications"
      (_: prev: {
        inherit (inputs.buildkit-tekton.packages.${prev.system}) tkn-local;
        inherit (inputs.dagger.packages.${prev.system}) dagger;
      })
    ];
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };
}
